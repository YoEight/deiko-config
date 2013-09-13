{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Text.Deiko.Config.Internal where

import Control.Applicative (Applicative(..), (<$>))
import Control.Exception (Exception)
import Control.Monad (liftM)
import Control.Monad.Reader (MonadReader(..), asks)
import Data.Foldable (Foldable, foldMap)
import Data.Hashable (hash)
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Traversable (Traversable, traverse)
import Data.Typeable (Typeable)
import Text.Deiko.Config.Util

data Token = Elm !Int !Int !Sym
           | EOF deriving Show

data Sym = ID !T.Text
         | STRING !T.Text
         | SUBST !T.Text
         | ERROR !T.Text
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

newtype ConfigError = ConfigError T.Text deriving (Show, Typeable)

instance Exception ConfigError

data AST name a = ASTRING !Position !T.Text
                | ALIST !Position [a]
                | ASUBST !Position !T.Text
                | AMERGE a a
                | AOBJECT !Position [Prop name a] deriving Show

data Prop name a = Prop { propName  :: !name
                        , propValue :: a } deriving Show

type Value s = Mu (AST s)

type Property name = Prop name (Value name)

type Untyped = Property Ident

type Typed = Property (T.Text, Type)

type Scoped = Property T.Text

type Register = I.IntMap (Type, Annoted)

data Ident = Ident !Position !T.Text
           | Select !Ident !Ident deriving Show

type TypeTable = I.IntMap Type

type Type = Mu TypeS

data TypeS a = TAny
             | TString
             | TObject
             | TList a
             | TRef !T.Text deriving Show

type Annoted = Value (T.Text, Type)

instance Functor TypeS where
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

castType :: TypeS a -> Type
castType TAny     = Mu TAny
castType TString  = Mu TString
castType TObject  = Mu TObject
castType (TRef s) = Mu (TRef s)

string :: Position -> T.Text -> Value a
string p x = Mu $ ASTRING p x

list :: Position -> [Value a] -> Value a
list p xs = Mu $ ALIST p xs

object :: Position -> [Property a] -> Value a
object p xs = Mu $ AOBJECT p xs

subst :: Position -> T.Text -> Value a
subst p x = Mu $ ASUBST p x

merge :: Value a -> Value a -> Value a
merge x y = Mu $ AMERGE x y

nil :: Position -> Value a
nil p = Mu $ ALIST p []

property :: a -> Value a -> Property a
property a v = Prop a v

resolveType :: MonadReader TypeTable m => Type -> m (Maybe Int)
resolveType = cata go
  where
    go TAny      = return (Just 1)
    go TString   = return (Just 2)
    go TObject   = return (Just 4)
    go (TList m) = liftM (fmap (*8)) m
    go (TRef s)  = resolveRef s

    resolveRef s =
      maybe (return Nothing) (cata go) =<< asks (I.lookup (hash s))

sameType :: MonadReader TypeTable m => Type -> Type -> m Bool
sameType x y = do
  rx <- resolveType x
  ry <- resolveType y
  return (rx == ry)

listOf :: Type -> Maybe Type
listOf typ = (cata go typ) False
  where
    go (TList k) False = k True
    go (TList k) b     = fmap listTypeOf (k b)
    go _ False         = Nothing
    go x _             = Just $ castType x

showType :: MonadReader TypeTable m => Type -> m T.Text
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

makeId :: Ident -> T.Text
makeId (Ident _ x)  = x
makeId (Select x y) = makeId x <> "." <> makeId y

pos :: Mu (AST a) -> Position
pos = cata go
  where
    go (ASTRING p _) = p
    go (ASUBST p _)  = p
    go (ALIST p _)   = p
    go (AMERGE p _)  = p
    go (AOBJECT p _) = p

stringType :: Type
stringType = Mu TString

objectType :: Type
objectType = Mu TObject

listTypeOf :: Type -> Type
listTypeOf = Mu . TList

nilType :: Type
nilType = listTypeOf (Mu TAny)

anyType :: Type
anyType = Mu TAny

refType :: T.Text -> Type
refType = Mu . TRef
