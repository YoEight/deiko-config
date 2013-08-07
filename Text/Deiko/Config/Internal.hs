{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Text.Deiko.Config.Internal where

import Control.Monad (liftM)
import Control.Monad.Error
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Applicative (Applicative(..), (<$>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable, foldMap)
import qualified Data.Map as M
import qualified Data.IntMap as I
import Data.Hashable
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Traversable (Traversable, traverse)
import Text.Deiko.Config.Util

data Token s = Elm Int Int (Sym s)
             | EOF deriving Show

data Sym s = ID s
           | STRING s
           | SUBST s
           | ERROR s
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

data ConfigError s = ConfigError s deriving Show

data AST s name a = ASTRING Position s
                  | ALIST Position [a]
                  | ASUBST Position s
                  | AMERGE a a
                  | AOBJECT Position [Prop name a] deriving Show

data Prop name a = Prop { propName  :: name
                        , propValue :: a } deriving Show

type Value s a = Mu (AST s a)

type Property s name = Prop name (Value s name)

type Untyped s = Property s (Ident s)

type Typed s = Property s (s, Type s)

type Scoped s = Property s s

type Register s = I.IntMap (Type s, Annoted s)

data Ident s = Ident Position s
             | Select (Ident s) (Ident s) deriving Show

type TypeTable s = I.IntMap (Type s)

type Type s = Mu (TypeS s)

data TypeS s a = TAny
               | TString
               | TObject
               | TList a
               | TRef s deriving Show

type Annoted s = Value s (s, Type s)

class (IsString s, Monoid s, Show s, Hashable s, Eq s) => StringLike s

instance Functor (TypeS s) where
  fmap _ TAny      = TAny
  fmap _ TString   = TString
  fmap _ TObject   = TObject
  fmap f (TList a) = TList (f a)
  fmap _ (TRef s)  = TRef s

instance Functor (Prop i) where
  fmap f (Prop id a) = Prop id (f a)

instance Foldable (Prop i) where
  foldMap f = f . propValue

instance Traversable (Prop i) where
  traverse f (Prop id a) = fmap (Prop id) (f a)

instance Functor (AST s i) where
  fmap f (ASTRING p x)   = ASTRING p x
  fmap f (ALIST p xs)    = ALIST p (fmap f xs)
  fmap f (ASUBST p x)    = ASUBST p x
  fmap f (AMERGE x y)    = AMERGE (f x) (f y)
  fmap f (AOBJECT p xs)  = AOBJECT p (fmap (fmap f) xs)

instance Foldable (AST s i) where
  foldMap _ (ASTRING _ _)  = mempty
  foldMap f (ALIST _ xs)   = foldMap f xs
  foldMap _ (ASUBST _ _)   = mempty
  foldMap f (AMERGE x y)   = f x `mappend` f y
  foldMap f (AOBJECT _ xs) = foldMap (foldMap f) xs

instance Traversable (AST s i) where
  traverse f (ALIST p xs)   = fmap (ALIST p) (traverse f xs)
  traverse f (AMERGE x y)   = AMERGE <$> f x <*> f y
  traverse f (AOBJECT p xs) = fmap (AOBJECT p) (traverse (traverse f) xs)
  traverse f (ASTRING p x)  = pure (ASTRING p x)
  traverse f (ASUBST p x)   = pure (ASUBST p x)

instance IsString s => Error (ConfigError s) where
  noMsg  = ConfigError "Panic"
  strMsg = ConfigError . fromString

instance StringLike String
instance StringLike TS.Text
instance StringLike TL.Text
instance StringLike BS.ByteString
instance StringLike BL.ByteString

castType :: TypeS a b -> Type a
castType TAny     = Mu TAny
castType TString  = Mu TString
castType TObject  = Mu TObject
castType (TRef s) = Mu (TRef s)

string :: IsString s => Position -> s -> Value s a
string p x = Mu $ ASTRING p x

list :: Position -> [Value s a] -> Value s a
list p xs = Mu $ ALIST p xs

object :: Position -> [Property s a] -> Value s a
object p xs = Mu $ AOBJECT p xs

subst :: Position -> s -> Value s a
subst p x = Mu $ ASUBST p x

merge :: Value s a -> Value s a -> Value s a
merge x y = Mu $ AMERGE x y

nil :: Position -> Value s a
nil p = Mu $ ALIST p []

property :: a -> Value s a -> Property s a
property a v = Prop a v

resolveType :: (MonadReader (TypeTable s) m, Hashable s)
            => Type s
            -> m (Maybe Int)
resolveType = cata go
  where
    go TAny      = return (Just 1)
    go TString   = return (Just 2)
    go TObject   = return (Just 4)
    go (TList m) = liftM (fmap (*8)) m
    go (TRef s)  = resolveRef s

    resolveRef s =
      maybe (return Nothing) (cata go) =<< asks (I.lookup (hash s))

sameType :: (MonadReader (TypeTable s) m, Hashable s)
         => Type s
         -> Type s
         -> m Bool
sameType x y = do
  rx <- resolveType x
  ry <- resolveType y
  return (rx == ry)

listOf :: Type s -> Maybe (Type s)
listOf typ = (cata go typ) False
  where
    go (TList k) False = k True
    go (TList k) b     = fmap listTypeOf (k b)
    go _ False         = Nothing
    go x _             = Just $ castType x

showType :: (MonadReader (TypeTable s) m, IsString s, Hashable s, Monoid s)
         => Type s
         -> m s
showType = cata go
  where
    go TAny      = return "A"
    go TString   = return "String"
    go TObject   = return "Object"
    go (TList m) = showList =<< m
    go (TRef s)  = showRef s

    showList t = return ("List[" <> t <> "]")

    showRef s =
      let deft = return ("typeof(" <> s <> ")") in
      (maybe deft (cata go)) =<< asks (I.lookup (hash s))

showPos :: Position -> String
showPos (l, c) = "(line: " ++ show l ++ ", col: " ++ show c ++ ")"

makeId :: (IsString s, Monoid s) => Ident s -> s
makeId (Ident _ x)  = x
makeId (Select x y) = makeId x <> "." <> makeId y

pos :: Mu (AST s a) -> Position
pos = cata go
  where
    go (ASTRING p _) = p
    go (ASUBST p _)  = p
    go (ALIST p _)   = p
    go (AMERGE p _)  = p
    go (AOBJECT p _) = p

stringType :: Type s
stringType = Mu TString

objectType :: Type s
objectType = Mu TObject

listTypeOf :: Type s -> Type s
listTypeOf = Mu . TList

nilType :: Type s
nilType = listTypeOf (Mu TAny)

anyType :: Type s
anyType = Mu TAny

refType :: IsString s => s -> Type s
refType = Mu . TRef
