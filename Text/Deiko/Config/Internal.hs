{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Deiko.Config.Internal where

import Control.Monad (liftM)
import Control.Monad.Error
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable, foldMap)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Hashable
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable, traverse)
import Text.Deiko.Config.Util

data Token = Elm Int Int Sym
           | EOF deriving Show

data Sym = ID String
         | STRING String
         | SUBST String
         | ERROR String
         | LBRACE
         | RBRACE
         | LBRACK
         | RBRACK
         | EQUAL
         | SPACE
         | NEWLINE
         | COMMA
         | DOT deriving Show

type Position = (Int, Int)

data ConfigError = ConfigError String deriving Show

data AST name a = ASTRING Position String
                | ALIST Position [a]
                | ASUBST Position String
                | AMERGE a a
                | AOBJECT Position [Prop name a] deriving Show

data Prop name a = Prop { propName  :: name
                        , propValue :: a } deriving Show

type Value a = Mu (AST a)

type Property name = Prop name (Value name)

type Register = I.IntMap (Type, Value (String, Type))

data Ident = Ident Position String
           | Select Ident Ident deriving Show

type TypeTable = I.IntMap Type

type Type = Mu (Cons (String, Bool, Int))

instance Functor (Prop i) where
  fmap f (Prop id a) = Prop id (f a)

instance Foldable (Prop i) where
  foldMap f = f . propValue

instance Traversable (Prop i) where
  traverse f (Prop id a) = fmap (Prop id) (f a)

instance Functor (AST i) where
  fmap f (ASTRING p x)   = ASTRING p x
  fmap f (ALIST p xs)    = ALIST p (fmap f xs)
  fmap f (ASUBST p x)    = ASUBST p x
  fmap f (AMERGE x y)    = AMERGE (f x) (f y)
  fmap f (AOBJECT p xs)  = AOBJECT p (fmap (fmap f) xs)

instance Foldable (AST i) where
  foldMap _ (ASTRING _ _)  = mempty
  foldMap f (ALIST _ xs)   = foldMap f xs
  foldMap _ (ASUBST _ _)   = mempty
  foldMap f (AMERGE x y)   = f x `mappend` f y
  foldMap f (AOBJECT _ xs) = foldMap (foldMap f) xs

instance Traversable (AST i) where
  traverse f (ALIST p xs)   = fmap (ALIST p) (traverse f xs)
  traverse f (AMERGE x y)   = AMERGE <$> f x <*> f y
  traverse f (AOBJECT p xs) = fmap (AOBJECT p) (traverse (traverse f) xs)
  traverse f (ASTRING p x)  = pure (ASTRING p x)
  traverse f (ASUBST p x)   = pure (ASUBST p x)

instance Error ConfigError where
  noMsg  = ConfigError "Panic"
  strMsg = ConfigError

string :: Position -> String -> Value a
string p x = Mu $ ASTRING p x

list :: Position -> [Value a] -> Value a
list p xs = Mu $ ALIST p xs

object :: Position -> [Property a] -> Value a
object p xs = Mu $ AOBJECT p xs

subst :: Position -> String -> Value a
subst p x = Mu $ ASUBST p x

merge :: Value a -> Value a -> Value a
merge x y = Mu $ AMERGE x y

nil :: Position -> Value a
nil p = Mu $ ALIST p []

property :: a -> Value a -> Property a
property a v = Prop a v

resolveType :: MonadReader TypeTable m => Type -> m (Maybe Int)
resolveType typ = (cata go typ) 0
  where
    go Nil hascode = return (Just hascode)
    go (Cons (_, ref, h) k) hashcode
       | ref       = step h hashcode
       | otherwise = k (hash (h, hashcode))

    step h hashcode =
      (maybe (return Nothing) (($ hashcode) . cata go)) =<< asks (I.lookup h)

sameType :: MonadReader TypeTable m => Type -> Type -> m Bool
sameType x y = do
  rx <- resolveType x
  ry <- resolveType y
  return (rx == ry)

showType :: MonadReader TypeTable m => Type -> m String
showType = cata go
  where
    go Nil = return ""
    go (Cons (lbl, ref, h) k)
       | ref       = step h lbl
       | otherwise =
         let f s
               | null s    = lbl
               | otherwise = lbl ++ "[" ++ s  ++ "]" in
         liftM f k

    step h lbl =
      let str = "typeof(" ++ lbl ++ ")" in
      (maybe (return str) (cata go)) =<< asks (I.lookup h)

showPos :: Position -> String
showPos (l, c) = "(line: " ++ show l ++ ", col: " ++ show c ++ ")"

makeId :: Ident -> String
makeId (Ident _ x)  = x
makeId (Select x y) = (makeId x) ++ "." ++ (makeId y)

pos :: Mu (AST a) -> Position
pos = cata go
  where
    go (ASTRING p _) = p
    go (ASUBST p _)  = p
    go (ALIST p _)   = p
    go (AMERGE p _)  = p
    go (AOBJECT p _) = p

stringCode :: Int
stringCode = hash "String"

objectCode :: Int
objectCode = hash "Object"

listCode :: Int
listCode = hash "List"

stringType :: Type
stringType = singleType "String"

objectType :: Type
objectType = singleType "Object"

listType :: Type
listType = singleType "List"

listTypeOf :: Type -> Type
listTypeOf t = higherType t listType

anyType :: Type
anyType = Mu (Cons ("A", False, hash "*") (Mu Nil))

anyValCode :: Int
anyValCode = (hash "*")

singleType :: String -> Type
singleType i = Mu (Cons (i, False, hash i) (Mu Nil))

refType :: String -> Type
refType i = Mu (Cons (i, True, hash i) (Mu Nil))

higherType :: Type -> Type -> Type
higherType i = cata go
  where
    go (Cons h t) = Mu (Cons h t)
    go Nil        = i
