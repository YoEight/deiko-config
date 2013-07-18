module Text.Deiko.Config.Internal where

import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable, traverse)


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

data AST a = ASTRING Position String
           | ALIST Position [a]
           | ASUBST Position String
           | AMERGE a a
           | AOBJECT Position [Prop a] deriving Show

data Prop a = Prop { propId    :: Ident
                   , propValue :: a } deriving Show

data Ident = Ident Position String
           | Select Ident Ident deriving Show

instance Functor Prop where
  fmap f (Prop id a) = Prop id (f a)

instance Foldable Prop where
  foldMap f = f . propValue

instance Traversable Prop where
  traverse f (Prop id a) = fmap (Prop id) (f a)

instance Functor AST where
  fmap f (ASTRING p x)   = ASTRING p x
  fmap f (ALIST p xs)    = ALIST p (fmap f xs)
  fmap f (ASUBST p x)    = ASUBST p x
  fmap f (AMERGE x y)    = AMERGE (f x) (f y)
  fmap f (AOBJECT p xs)  = AOBJECT p (fmap (fmap f) xs)

instance Foldable AST where
  foldMap _ (ASTRING _ _)  = mempty
  foldMap f (ALIST _ xs)   = foldMap f xs
  foldMap _ (ASUBST _ _)   = mempty
  foldMap f (AMERGE x y)   = f x  `mappend` f y
  foldMap f (AOBJECT _ xs) = foldMap (foldMap f) xs 

instance Traversable AST where
  traverse f (ALIST p xs)   = fmap (ALIST p) (traverse f xs)
  traverse f (AMERGE x y)   = AMERGE <$> f x <*> f y
  traverse f (AOBJECT p xs) = fmap (AOBJECT p) (traverse (traverse f) xs)
  traverse f (ASTRING p x)  = pure (ASTRING p x)
  traverse f (ASUBST p x)   = pure (ASUBST p x)
