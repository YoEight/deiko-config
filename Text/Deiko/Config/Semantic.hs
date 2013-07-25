module Text.Deiko.Config.Semantic (Type(..)
                                  ,Config(..)
                                  ,Register
                                  ,typecheck
                                  , showType) where

import Prelude hiding (sequence)
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad.Error hiding (sequence)
import Control.Monad.State (StateT, State, execState, evalState, evalStateT)
import Control.Monad.RWS (RWS, modify, get, gets, ask, asks, runRWS)
import Data.Conduit (Conduit, awaitForever, yield, (=$=))
import Data.Foldable (traverse_, foldMap)
import qualified Data.Map as M
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse, sequence)
import Text.Deiko.Config.Util
import Text.Deiko.Config.Internal

type Scoping = String -> Mu (AST String)
type Typing m = StateT TypeState (ErrorT ConfigError m) (Type, Mu (AST (String, Type)))
type Registering = State Register (Mu (AST (String, Type)))
type Phase a m o = Conduit a (ErrorT ConfigError m) o

data Constraint = Constraint Type Type deriving Show

data TypeState = TypeState { tsType        :: M.Map String Type
                           , tsConstraints :: [Constraint] } deriving Show

data Config = Config { cReg     :: Register
                     , cState   :: TypeState } deriving Show

typecheck :: (Functor m, Monad m) => Phase [Property Ident] m Config
typecheck = typingPhase =$= withDefaultReg =$= checkingPhase

typingPhase :: (Functor m, Monad m)
            => Phase [Property Ident] m ([Property (String, Type)], TypeState)
typingPhase = awaitForever go
  where
    go props =
      let props2 = fmap scoping props
          action = (,) <$> traverse typing props2 <*> get
          action2 = lift $ evalStateT action (TypeState M.empty []) in
      action2 >>= yield

withDefaultReg :: Monad m => Phase (a, b) m (a, b, Register)
withDefaultReg = awaitForever go
  where
    go (a, b) = yield (a, b, M.empty)

checkingPhase :: Monad m
              => Phase ([Property (String, Type)], TypeState, Register) m Config
checkingPhase = awaitForever go
  where
    go (props, state, reg) =
      let reg2         = execState (traverse register props) reg
          step constrs = yield $ Config reg2 state{tsConstraints=constrs} in
      lift (checking state) >>= step

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

check :: Constraint -> RWS (M.Map String Type) () [Constraint] String
check (Constraint (TString _) (TString _))   = return ""
check (Constraint (TObject _) (TObject _))   = return ""
check (Constraint (TNil _) (TList _ _))      = return ""
check (Constraint (TList _ _) (TNil _))      = return ""
check (Constraint (TNil _) (TNil _))         = return ""
check (Constraint (TList px x) (TList py y)) = checkList (px, x) (py, y)
check (Constraint (TVar p x) typ)            = checkSubst (p, x) typ
check c                                      = checkFail c

checkList :: (Position, Type)
          -> (Position, Type)
          -> RWS (M.Map String Type) () [Constraint] String
checkList (px, x) (py, y) = fmap go (check (Constraint x y))
  where
    go msg
      | null msg  = msg
      | otherwise = listCheckError px msg

checkSubst :: (Position, String)
           -> Type
           -> RWS (M.Map String Type) () [Constraint] String
checkSubst (p, x) y =
  ask >>= \types ->
    let step = \x y -> check (Constraint x y)
        resx = fmap (updatePos p) (M.lookup x types)
        resy = case y of
                 TVar py ky -> fmap (updatePos py) (M.lookup ky types)
                 _          -> Just y
        unresolved = fmap (const "") (modify (Constraint (TVar p x) y:))
        action     = step <$> resx <*> resy in
    maybe unresolved id action

checkFail :: Constraint -> RWS (M.Map String Type) () [Constraint] String
checkFail (Constraint x y) = return msg
  where
     xlabel = showType x
     ylabel = showType y
     xpos   = showPos $ position x
     ypos   = showPos $ position y
     msg    = "Expected " ++
              xlabel ++ " " ++ xpos ++
              " but having " ++ ylabel ++
              " " ++ ypos ++ "\n"

listCheckError :: Position -> String -> String
listCheckError p e = "In context of List " ++ showPos p ++ ": " ++ e

checking :: Monad m => TypeState -> ErrorT ConfigError m [Constraint]
checking (TypeState types constrs) =
  let action          = fmap mconcat (traverse check constrs)
      (msg, unres, _) = runRWS action types [] in
  if null msg then return unres else throwError (ConfigError msg)

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
