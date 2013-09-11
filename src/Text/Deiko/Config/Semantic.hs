{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
module Text.Deiko.Config.Semantic
  ( Config(..)
  , TypeState(..)
  , Register
  , typecheck
  , showType
  , manualSimplify
  ) where

import Prelude hiding (sequence)
import Control.Arrow ((***))
import Control.Applicative (Applicative (..), (<$>))
import Control.Monad.Error hiding (sequence)
import Control.Monad.State (StateT, State, execState, evalState, evalStateT
                           , execStateT)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.RWS (RWS, modify, get, gets, ask, asks, runRWS, put)
import Data.Conduit (Conduit, Source, awaitForever, yield, (=$=))
import Data.Foldable (traverse_, foldMap)
import qualified Data.IntMap as I
import Data.Hashable
import qualified Data.Map as M
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Traversable (traverse, sequence)
import Data.Tuple (swap)
import Text.Deiko.Config.Util
import Text.Deiko.Config.Internal

type Scoping s = s -> Value s
type Registering s = State Register Annoted
type Phase a o =
  forall m. (Functor m, MonadError ConfigError m) => Conduit a m o

data Constraint = Equal (Position, Type) (Position, Type) deriving Show

data TypeState = TypeState { tsTable       :: TypeTable
                           , tsConstraints :: [Constraint] } deriving Show

data Config = Config { cReg   :: Register
                     , cState :: TypeState } deriving Show

typecheck :: Phase [Untyped] Config
typecheck = typingPhase =$= withDefaultReg =$= checkingPhase =$= simplifyPhase

typingPhase :: Phase [Untyped] ([Typed], TypeState)
typingPhase = awaitForever go
  where
    go props =
      let props2  = fmap scoping props
          action  = (,) <$> traverse typing props2 <*> get
          action2 = lift $ evalStateT action (TypeState I.empty []) in
      action2 >>= yield

manualSimplify :: (MonadError ConfigError m, Functor m)
               => Config
               -> Source m Config
manualSimplify config =
  yield config =$= simplifyPhase

withDefaultReg :: Phase (a, b) (a, b, Register)
withDefaultReg = awaitForever go
  where
    go (a, b) = yield (a, b, I.empty)

checkingPhase :: Phase ([Typed], TypeState, Register) Config
checkingPhase = awaitForever go
  where
    go (props, state, reg) =
      let reg2    = execState (traverse register props) reg
          step xs = yield $ Config reg2 state{tsConstraints=xs} in
      lift (checking state) >>= step

simplifyPhase :: Phase Config Config
simplifyPhase = awaitForever go
  where
    go (Config reg ts) =
      let tbl    = tsTable ts
          action = traverse_ simplify (I.toList reg) in
      do (tbl2, reg2) <- execStateT action (tbl, reg)
         yield (Config reg2 ts{tsTable=tbl2})

scoping :: Untyped -> Scoped
scoping (Prop id ast) = Prop key ((para scopingAST ast) (key <> "."))
  where
    key = makeId id

    scopingAST (ASTRING p x)            = const $ string p x
    scopingAST (ASUBST p x)             = const $ subst p x
    scopingAST (ALIST p xs)             = scopingList p xs
    scopingAST (AMERGE (_, fx) (_, fy)) = scopingMerge fx fy
    scopingAST (AOBJECT p xs)           = scopingObject p xs

    scopingList p [] _              = nil p
    scopingList p ((Mu ast, _):_) _ =
      case ast of
        ALIST _ as -> list p (fmap (\a -> para scopingAST a "") as)

    scopingMerge fx fy scope = merge (fx scope) (fy scope)

    scopingObject p props scope =
      object p (fmap step props')
        where
          props' = fmap (fmap snd) props
          step (Prop id f) =
            let key = scope <> makeId id in
            property key (f (key <> "."))

typing :: (MonadError ConfigError m, Functor m)
       => Scoped
       -> StateT TypeState m Typed
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

      typingMerge x y = do
        (tx, x1) <- x
        (ty, y1) <- y
        modify (constrs (pos x1, tx) (pos y1, ty))
        return (tx, merge x1 y1)
          where
            constrs x y (TypeState m cs) =
              TypeState m (Equal x y:cs)

      typingList  p xs =
        sequence xs >>= \xstype ->
          case xstype of
            []           -> return (nilType, nil p)
            ((t, at):ts) ->
              let typ   = listTypeOf t
                  step  = swap . (id *** pos)
                  constrs (TypeState m cs0) =
                    TypeState m (cs0 ++ fmap (Equal (pos at, t) . step) ts) in
              modify constrs >>
              return (typ, list p (fmap snd xstype))

      typingObject p xs =
        liftM (\xs1 -> (objectType, object p xs1)) (traverse step xs)
          where
            step (Prop key x) = liftM (\(t, ast1) -> (Prop (key, t) ast1)) x

check :: Constraint -> RWS TypeTable () [Constraint] T.Text
check c@(Equal (px, tx) (py, ty)) =
  resolveType tx >>= \rx ->
    resolveType ty >>= \ry ->
      case () of
        _ | rx == ry  -> return ""
          | otherwise ->
            let failure _ = checkFail (px, tx) (py, ty) in
            maybe (unresolved c) failure (rx >> ry)

unresolved :: Constraint -> RWS TypeTable () [Constraint] T.Text
unresolved c = fmap (const "") (modify (c:))

checkFail :: (Position, Type)
          -> (Position, Type)
          -> RWS TypeTable () a T.Text
checkFail (px, symx) (py, symy) = do
  xlabel <- showType symx
  ylabel <- showType symy
  return (msg xlabel ylabel)
  where
    xpos  = fromString $ showPos px
    ypos  = fromString $ showPos py
    msg xlabel ylabel = "Expected " <>
                        xlabel <> " " <> xpos <>
                        " but having " <> ylabel <>
                        " " <> ypos <> "\n"

checking :: (MonadError ConfigError m)
         => TypeState
         -> m [Constraint]
checking (TypeState types constrs) =
  let action          = fmap mconcat (traverse check constrs)
      (msg, unres, _) = runRWS action types []
      isEmpty x = x == mempty in
  if isEmpty msg then return unres else throwError (ConfigError msg)

register :: Typed -> State Register ()
register (Prop (key, typ) ast) =
  cata registerAST ast >>= \ast1 ->
    modify (I.insert (hash key) (typ, ast1))
    where
      registerAST (ASTRING p x)     = return $ string p x
      registerAST (ASUBST p x)      = return $ subst p x
      registerAST (ALIST p xs)      = fmap (list p) (sequence xs)
      registerAST (AMERGE x y)      = merge <$> x <*> y
      registerAST (AOBJECT p props) = registerObject p props

      registerObject p props = fmap (object p) (traverse go props)
        where
          go (Prop (key, typ) f) =
            f >>= \ast ->
              modify (I.insert (hash key) (typ, ast)) >>
              return (Prop (key, typ) ast)

simplify :: (Monad m, Functor m)
         => (Int, (Type, Value (T.Text, Type)))
         -> StateT (TypeTable, Register) m ()
simplify (key, (typ, value)) =
  get >>= \(table, reg) ->
    let resolved =
          maybe False (const True) (runReader (resolveType typ) table) in
    case () of
      _ | resolved  ->
          simplifying key value >>= \(typ2, value2) ->
            let table2 = I.insert key typ2 table
                reg2   = I.insert key (typ2, value2) reg in
            put (table2, reg2)
        | otherwise -> return ()

simplifying :: (Monad m, Functor m)
            => Int
            -> Annoted
            -> StateT (TypeTable, Register) m (Type, Annoted)
simplifying key value = cata simplifyAST value
  where
    simplifyAST (ASTRING p s)  = return (stringType, string p s)
    simplifyAST (ALIST p vs)   = simplifyList p vs
    simplifyAST (AMERGE fx fy) = merging <$> fx <*> fy
    simplifyAST (AOBJECT p xs) = simplifyObject p xs
    simplifyAST (ASUBST p x)   = simplifySubst p x

    simplifyList p vs =
      sequence vs >>= \xs ->
        let typ =
              case () of
                _ | null xs   -> anyType
                  | otherwise -> fst $ head xs in
        return (listTypeOf typ, list p (fmap snd xs))

    simplifyObject p xs =
      traverse sequence xs >>= \props ->
        return (objectType, object p (fmap (fmap snd) props))

    simplifySubst p x =
      get >>= \(table, reg) ->
        let key2 = hash x
            (Just (_, value2)) = I.lookup key2 reg in
        simplifying key2 value2 >>= \(typ2, value3) ->
          let table2 = I.insert key2 typ2 table
              reg2   = I.insert key2 (typ2, value3) reg in
          fmap (const (typ2, value3)) (put (table2, reg2))

merging :: (Type, Annoted) -> (Type, Annoted) -> (Type, Annoted)
merging (tx, vx) (_, vy) =
  case (vx, vy) of
    (Mu (ASTRING p xs), Mu (ASTRING _ ys)) -> (tx, string p (xs <> " " <> ys))
    (Mu (ALIST p xs), Mu (ALIST _ ys))     -> (tx, list p (xs ++ ys))
    (Mu (AOBJECT p xs), Mu (AOBJECT _ ys)) -> (tx, object p (xs ++ ys))
