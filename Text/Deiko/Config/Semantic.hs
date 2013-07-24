module Text.Deiko.Config.Semantic (Type(..), Register, typecheck, showType) where

import Prelude hiding (sequence)
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad (liftM)
import Control.Monad.Error hiding (sequence)
import Control.Monad.State (StateT, State, modify, gets, get
                           , execState, evalState, evalStateT)
import Control.Monad.Trans (lift)
import Data.Conduit (Conduit, awaitForever, yield)
import Data.Foldable (traverse_, foldMap)
import qualified Data.Map as M
import Data.Traversable (traverse, sequence)
import Text.Deiko.Config.Util
import Text.Deiko.Config.Internal

type Scoping = String -> Mu (AST String)
type Typing m = StateT TypeState (ErrorT ConfigError m) (Type, Mu (AST (String, Type)))
type Registering = State Register (Mu (AST (String, Type)))

data Constraint = Constraint Type Type deriving Show

data TypeState = TypeState { tsType        :: M.Map String Type
                           , tsConstraints :: [Constraint] } deriving Show

typecheck :: (Functor m, Monad m) 
          => Conduit [Property Ident] (ErrorT ConfigError m) Register
typecheck = awaitForever ((yield =<<) . lift . semantic)

semantic :: (Functor m, Monad m)
         => [Property Ident]
         -> ErrorT ConfigError m Register
semantic props = 
  let props2 = fmap scoping props
      action = traverse typing props2 >>= \props3 ->
               fmap (const $ execState (traverse register props3) M.empty)
                    checking in
  evalStateT action (TypeState M.empty [])

scoping :: Property Ident -> Property String
scoping (Prop id ast) = Prop key ((para scopingAST ast) (key ++ "."))
  where
    key = makeId id

scopingAST :: AST Ident (Mu (AST Ident), Scoping) -> Scoping
scopingAST (ASTRING p x)            = const $ string p x
scopingAST (ASUBST p x)             = const $ subst p x
scopingAST (ALIST p xs)             = scopingList p xs
scopingAST (AMERGE (_, fx) (_, fy)) = scopingMerge fx fy
scopingAST (AOBJECT p xs)           = scopingObject p xs

scopingList :: Position -> [(Mu (AST Ident), a)] -> Scoping
scopingList p [] _              = nil p
scopingList p ((Mu ast, _):_) _ = 
  case ast of 
    ALIST _ as -> list p (fmap (\a -> para scopingAST a "") as)

scopingMerge :: Scoping -> Scoping -> Scoping
scopingMerge fx fy scope = merge (fx scope) (fy scope)

scopingObject :: Position 
              -> [Prop Ident (Mu (AST Ident), Scoping)]
              -> Scoping
scopingObject p props scope = 
  object p (fmap step props')
    where
      props' = fmap (fmap snd) props
      step (Prop id f) =
        let key = scope ++ makeId id in
        property key (f (key ++ "."))

typing :: (Monad m, Functor m) 
       => Property String 
       -> StateT TypeState (ErrorT ConfigError m) (Property (String, Type))
typing (Prop key ast) = do
  (t, ast1) <- cata typingAST ast
  modify (\s@TypeState{tsType=m} -> s{tsType=M.insertWith const key t m})
  return (Prop (key, t) ast1)
    where
      typingAST (ASTRING p x)  = return (TString p, string p x)
      typingAST (AMERGE x y)   = typingMerge x y
      typingAST (ASUBST p x)   = typingSubst p x
      typingAST (ALIST p xs)   = typingList p xs
      typingAST (AOBJECT p xs) = typingObject p xs

typingMerge :: Monad m => Typing m -> Typing m -> Typing m
typingMerge x y = do
  (tx, x1) <- x
  (ty, y1) <- y
  constrs  <- gets tsConstraints
  modify (\s -> s{tsConstraints=(Constraint tx ty:constrs)})
  return (tx, merge x1 y1)

typingSubst :: Monad m => Position -> String -> Typing m
typingSubst p x = liftM go (gets tsType)
  where
    go m = (maybe (TVar p x) (updatePos p) (M.lookup x m), subst p x)

typingList :: Monad m => Position -> [Typing m] -> Typing m
typingList  p xs = do
  xstype <- sequence xs
  case xstype of
    []         -> return (TNil p, nil p)
    ((t,_):ts) -> 
      let step s@TypeState{tsConstraints=cs0} =
            s{tsConstraints=cs0 ++ fmap (Constraint t . fst) ts}  in
      modify step >>
      return (TList p t, list p (fmap snd xstype))

typingObject :: (Monad m, Functor m) 
             => Position 
             -> [Prop String (Typing m)]
             -> Typing m
typingObject p xs = 
  liftM (\xs1 -> (TObject p, object p xs1)) (traverse step xs)
  where
    step (Prop key x) = liftM (\(t, ast1) -> (Prop (key, t) ast1)) x

checking :: Monad m => StateT TypeState (ErrorT ConfigError m) ()
checking = do
  (TypeState types constrs) <- get
  case foldMap (go types) constrs of
    []  -> return ()
    msg -> throwError (ConfigError msg) 
    where
      go types (Constraint x y) =
        case (x, y) of
          (TString _, TString _) -> ""
          (TObject _, TObject _) -> ""
          (TNil _, TList _ _)    -> ""
          (TList _ _, TNil _)    -> ""
          (TNil _, TNil _)       -> ""
          (TVar px x, TVar py y) ->
            let action =
                  (\xtype ytype -> go types (Constraint xtype ytype)) <$>
                  fmap (updatePos px) (M.lookup x types)              <*>
                  fmap (updatePos py) (M.lookup y types) in
            maybe [] id action
          (TList p x, TList _ y) -> 
            let msg = go types (Constraint x y) 
                ctx = "In context of List " ++ showPos p ++ " " in
            if null msg then msg else ctx ++ msg
          (x, y) ->
            let xlabel = showType x
                ylabel = showType y
                xpos   = showPos $ position x
                ypos   = showPos $ position y
                msg    = "Expected " ++ 
                         xlabel ++ " " ++ xpos ++ 
                         " but having " ++ ylabel ++ 
                         " " ++ ypos ++ "\n" in
            msg

register :: Property (String, Type) -> State Register ()
register (Prop (key, typ) ast) =
  cata registerAST ast >>= \ast1 ->
    modify (M.insertWith const key (typ, ast1))
    where
      registerAST (ASTRING p x)     = return $ string p x
      registerAST (ASUBST p x)      = registerSubst p x
      registerAST (ALIST p xs)      = fmap (list p) (sequence xs) 
      registerAST (AMERGE x y)      = registerMerge x y
      registerAST (AOBJECT p props) = registerObject p props

registerSubst :: Position -> String -> Registering
registerSubst p x = liftM (maybe (subst p x) (snd . id)) (gets (M.lookup x))
        
registerMerge :: Registering -> Registering -> Registering
registerMerge x y = go <$> x <*> y
  where
    go xtype ytype =
      case (out xtype, out ytype) of
        (ALIST p vs, ALIST _ ws)     -> list p (vs ++ ws)
        (ASTRING p v, ASTRING _ w)   -> string p (v ++  " " ++ w)
        (AOBJECT p vs, AOBJECT _ ws) -> object p (vs ++ ws)
        _                            -> merge xtype ytype

registerObject :: Position -> [Prop (String, Type) Registering] -> Registering
registerObject p props = fmap (object p) (traverse go props)
  where
    go (Prop (key, typ) f) = do
      ast <- f
      modify (M.insertWith const key (typ, ast))
      return (Prop (key, typ) ast)
