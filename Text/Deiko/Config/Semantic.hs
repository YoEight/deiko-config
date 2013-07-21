module Text.Deiko.Config.Semantic where

import Prelude hiding (sequence)
import Control.Applicative
import Control.Monad (when, (<=<), void)
import Control.Monad.State (StateT, State, modify, gets, get
                           , execState, evalState, evalStateT)
import Control.Monad.Trans (lift)
import Data.Conduit
import Data.Foldable (traverse_, foldMap)
import qualified Data.Map                 as M
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse, sequence)
import Text.Deiko.Config.Util
import Text.Deiko.Config.Internal

data Type = TString Position
          | TVar Position String
          | TList Position Type
          | TNil Position
          | TObject Position deriving Show

data Constraint = Constraint Type Type deriving Show

data TypeState = TypeState { tsType        :: M.Map String Type
                           , tsConstraints :: [Constraint] } deriving Show

type Register = M.Map String (Type, Mu (AST (String, Type)))

typecheck :: Monad m 
          => Conduit (Either String [Property Ident]) m (Either String Register)
typecheck = awaitForever (yield . (semantic =<<))

semantic :: [Property Ident] -> Either String Register
semantic props = 
  let props2 = fmap scoping props
      action = 
        do props3 <- traverse typing props2
           checking
           return (execState (traverse register props3) M.empty) in
  evalStateT action (TypeState M.empty [])      

scoping :: Property Ident -> Property String
scoping (Prop id ast) = Prop key ((para scopingAST ast) (key ++ "."))
  where
    key = makeId id
          
    scopingAST (ASTRING p x) _ = Mu $ ASTRING p x
    scopingAST (ASUBST p x)  _ = Mu $ ASUBST p x
    scopingAST (ALIST p xs)  _
      | null xs   = Mu $ ALIST p []
      | otherwise = 
        let (Mu (ALIST p as), _) = head xs in
        Mu $ ALIST p (fmap (\a -> para scopingAST a "") as)
    scopingAST (AMERGE (_, fx) (_, fy)) scope =
      Mu $ AMERGE (fx scope) (fy scope)
    scopingAST (AOBJECT p xs) scope = 
      let xs'              = fmap (fmap snd) xs
          step (Prop id f) =
            let key = scope ++ makeId id in
            Prop key (f (key ++ ".")) in
      Mu $ AOBJECT p (fmap step xs')

typing :: (Monad m, Functor m) 
       => Property String 
       -> StateT TypeState m (Property (String, Type))
typing (Prop key ast) = do
  (t, ast1) <- cata typingAST ast
  modify (\s@TypeState{tsType=m} -> s{tsType=M.insertWith const key t m})
  return (Prop (key, t) ast1)
    where
      typingAST (ASTRING p x) = return (TString p, Mu $ ASTRING p x)
      typingAST (AMERGE x y)  = do
        (tx, x1) <- x
        (ty, y1) <- y
        constrs  <- gets tsConstraints
        modify (\s -> s{tsConstraints=(Constraint tx ty:constrs)})
        return (tx, Mu $ AMERGE x1 y1)
      typingAST (ASUBST p x)  = 
        fmap (\m -> (maybe (TVar p x) 
                             (updatePos p) 
                             (M.lookup x m), Mu $ ASUBST p x))
               (gets tsType)
      typingAST (ALIST p xs)  = do
        xstype <- sequence xs
        case xstype of
          []         -> return (TNil p, Mu $ ALIST p [])
          ((t,_):ts) -> 
            let step s@TypeState{tsConstraints=cs0} =
                  s{tsConstraints=cs0 ++ fmap (Constraint t . fst) ts}  in
            modify step >>
            return (TList p t, Mu $ ALIST p (fmap snd xstype))
      typingAST (AOBJECT p xs) =
        let step (Prop key x) =
              do (t, ast1) <- x
                 return (Prop (key, t) ast1) in
        fmap (\xs1 -> (TObject p, Mu $ AOBJECT p xs1)) (traverse step xs)

checking :: StateT TypeState (Either String) ()
checking = do
  (TypeState types constrs) <- get
  case foldMap (go types) constrs of
    []  -> lift $ Right ()
    msg -> lift $ Left msg 
    where
      go types (Constraint x y) =
        case (x, y) of
          (TString _, TString _) -> mempty
          (TObject _, TObject _) -> mempty
          (TNil _, TList _ _)    -> mempty
          (TList _ _, TNil _)    -> mempty
          (TNil _, TNil _)       -> mempty
          (TVar px x, TVar py y) ->
            let action = 
                  do xtype <- fmap (updatePos px) (M.lookup x types)
                     ytype <- fmap (updatePos py) (M.lookup y types)
                     return $ go types (Constraint xtype ytype) in
            maybe mempty id action
          (TList p x, TList _ y) -> 
            let msg = go types (Constraint x y) 
                ctx = "In context of List " ++ showPos p ++ " " in
            if null msg then mempty else ctx ++ msg
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
      registerAST (ASTRING p x) = return (Mu $ ASTRING p x)
      registerAST (ASUBST p x)  = return (Mu $ ASUBST p x)
      registerAST (ALIST p xs)  = fmap (Mu . ALIST p) (sequence xs) 
      registerAST (AMERGE x y)  =
        do xtype <- x
           ytype <- y
           case (out xtype, out ytype) of
             (ALIST p vs, ALIST _ ws)     -> return $ Mu $ ALIST p (vs ++ ws)
             (ASTRING p v, ASTRING _ w)   -> return $ Mu $ ASTRING p (v ++ w)
             (AOBJECT p vs, AOBJECT _ ws) -> return $ Mu $ AOBJECT p (vs ++ ws)
             _                            -> return $ Mu $ AMERGE xtype ytype
      registerAST (AOBJECT p props) =
        let step (Prop (key, typ) f) =
              do ast <- f
                 modify (M.insertWith const key (typ, ast))
                 return (Prop (key, typ) ast) in
        fmap (Mu . AOBJECT p) (traverse step props)

showType :: Type -> String
showType (TString _) = "String"
showType (TObject _) = "Object"
showType (TList _ t) = "List[" ++ showType t ++ "]"
showType (TVar _ x)  = "typeof(" ++ x ++ ")"

showPos :: Position -> String
showPos (l, c) = "(line: " ++ show l ++ ", col: " ++ show c ++ ")"

makeId :: Ident -> String
makeId (Ident _ x)  = x
makeId (Select x y) = (makeId x) ++ "." ++ (makeId y)

initState = TypeState M.empty []

position :: Type -> Position
position (TString p) = p
position (TVar p _)  = p
position (TList p _) = p
position (TNil p)    = p
position (TObject p) = p

updatePos :: Position -> Type -> Type
updatePos p (TString _) = TString p
updatePos p (TVar _ x)  = TVar p x
updatePos p (TList _ x) = TList p x
updatePos p (TNil _)    = TNil p
updatePos p (TObject _) = TObject p
