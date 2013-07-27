module Text.Deiko.Config.Semantic
  (
    Type(..)
  , Config(..)
  , TypeState(..)
  , Register
  , typecheck
  , showType
  ) where

import Prelude hiding (sequence)
import Control.Arrow ((***))
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad.Error hiding (sequence)
import Control.Monad.State (StateT, State, execState, evalState, evalStateT)
import Control.Monad.RWS (RWS, modify, get, gets, ask, asks, runRWS)
import Data.Conduit (Conduit, awaitForever, yield, (=$=))
import Data.Foldable (traverse_, foldMap)
import qualified Data.IntMap as I
import Data.Hashable
import qualified Data.Map as M
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse, sequence)
import Data.Tuple (swap)
import Text.Deiko.Config.Util
import Text.Deiko.Config.Internal

type Scoping = String -> Mu (AST String)
type Typing m = StateT TypeState (ErrorT ConfigError m) (Type, Mu (AST (String, Type)))
type Registering = State Register (Mu (AST (String, Type)))
type Phase a m o = Conduit a (ErrorT ConfigError m) o

data Constraint = Equal (Position, Type) (Position, Type)

data TypeState = TypeState { tsTable       :: TypeTable
                           , tsConstraints :: [Constraint] }

data Config = Config { cReg     :: Register
                     , cState   :: TypeState }

typeTable :: TypeTable
typeTable = I.fromList [(stringCode, stringType)
                       ,(objectCode, objectType)
                       ,(anyValCode, anyType)]

typecheck :: (Functor m, Monad m) => Phase [Property Ident] m Config
typecheck = typingPhase =$= withDefaultReg =$= checkingPhase

typingPhase :: (Functor m, Monad m)
            => Phase [Property Ident] m ([Property (String, Type)], TypeState)
typingPhase = awaitForever go
  where
    go props =
      let props2  = fmap scoping props
          action  = (,) <$> traverse typing props2 <*> get
          action2 = lift $ evalStateT action (TypeState typeTable []) in
      action2 >>= yield

withDefaultReg :: Monad m => Phase (a, b) m (a, b, Register)
withDefaultReg = awaitForever go
  where
    go (a, b) = yield (a, b, I.empty)

checkingPhase :: Monad m
              => Phase ([Property (String, Type)], TypeState, Register) m Config
checkingPhase = awaitForever go
  where
    go (props, state, reg) =
      let reg2    = execState (traverse register props) reg
          step xs = yield $ Config reg2 state{tsConstraints=xs} in
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
  modify (types t)
  return (Prop (key, t) ast1)
    where
      types t (TypeState ts m) =
        TypeState (I.insert (hash key) t ts) m

      typingAST (ASTRING p x)  = return (stringType, string p x)
      typingAST (AMERGE x y)   = typingMerge x y
      typingAST (ASUBST p x)   = return (refType x, subst p x)
      typingAST (ALIST p xs)   = typingList p xs
      typingAST (AOBJECT p xs) = typingObject p xs

typingMerge :: Monad m => Typing m -> Typing m -> Typing m
typingMerge x y = do
  (tx, x1) <- x
  (ty, y1) <- y
  modify (constrs (pos x1, tx) (pos y1, ty))
  return (tx, merge x1 y1)
    where
      constrs x y (TypeState m cs) =
        TypeState m (Equal x y:cs)

typingList :: Monad m => Position -> [Typing m] -> Typing m
typingList  p xs =
  sequence xs >>= \xstype ->
    case xstype of
      []           -> return (listTypeOf anyType, nil p)
      ((t, at):ts) ->
        let typ   = listTypeOf t
            step  = swap . (id *** pos)
            constrs (TypeState m cs0) =
              TypeState m (cs0 ++ fmap (Equal (pos at, t) . step) ts) in
        modify constrs >>
        return (typ, list p (fmap snd xstype))

typingObject :: (Monad m, Functor m)
             => Position
             -> [Prop String (Typing m)]
             -> Typing m
typingObject p xs =
  liftM (\xs1 -> (objectType, object p xs1)) (traverse step xs)
  where
    step (Prop key x) = liftM (\(t, ast1) -> (Prop (key, t) ast1)) x

check :: Constraint -> RWS TypeTable () [Constraint] String
check c@(Equal (px, tx) (py, ty)) =
  resolveType tx >>= \rx ->
    resolveType ty >>= \ry ->
      case () of
        _ | rx == ry  -> return ""
          | otherwise ->
            let failure _ = checkFail (px, tx) (py, ty) in
            maybe (unresolved c) failure (rx >> ry)

unresolved :: Constraint -> RWS TypeTable () [Constraint] String
unresolved c = fmap (const "") (modify (c:))

checkFail :: (Position, Type)
          -> (Position, Type)
          -> RWS TypeTable () a String
checkFail (px, symx) (py, symy) = do
  xlabel <- showType symx
  ylabel <- showType symy
  return (msg xlabel ylabel)
  where
    xpos  = showPos px
    ypos  = showPos py
    msg xlabel ylabel = "Expected " ++
                        xlabel ++ " " ++ xpos ++
                        " but having " ++ ylabel ++
                        " " ++ ypos ++ "\n"

cxtCheckError :: Position -> Symbol -> String -> String
cxtCheckError p sym e =
  "In context of " ++ symLbl sym ++ " " ++ showPos p ++ ": " ++ e

checking :: Monad m => TypeState -> ErrorT ConfigError m [Constraint]
checking (TypeState types constrs) =
  let action          = fmap mconcat (traverse check constrs)
      (msg, unres, _) = runRWS action types [] in
  if null msg then return unres else throwError (ConfigError msg)

register :: Property (String, Type) -> State Register ()
register (Prop (key, typ) ast) =
  cata registerAST ast >>= \ast1 ->
    modify (I.insert (symCode sym) (sym, (typ, ast1)))
    where
      sym = mkSymbol key

      registerAST (ASTRING p x)     = return $ string p x
      registerAST (ASUBST p x)      = return $ subst p x
      registerAST (ALIST p xs)      = fmap (list p) (sequence xs)
      registerAST (AMERGE x y)      = registerMerge x y
      registerAST (AOBJECT p props) = registerObject p props

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
    go (Prop (key, typ) f) =
      f >>= \ast ->
        let sym = mkSymbol key in
        modify (I.insert (symCode sym) (sym, (typ, ast))) >>
        return (Prop (key, typ) ast)
