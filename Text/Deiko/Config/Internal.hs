{-# LANGUAGE RankNTypes #-}
module Text.Deiko.Config.Internal where

import Control.Monad.Error
import Control.Applicative (Applicative(..), (<$>))
import Data.Foldable (Foldable, foldMap)
import qualified Data.Map as M
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

data Type = TString Position
          | TVar Position String
          | TList Position Type
          | TNil Position
          | TObject Position deriving Show

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

type Register = M.Map String (Type, Mu (AST (String, Type)))

data Ident = Ident Position String
           | Select Ident Ident deriving Show

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

showType :: Type -> String
showType (TString _) = "String"
showType (TObject _) = "Object"
showType (TList _ t) = "List[" ++ showType t ++ "]"
showType (TNil _)    = "List[A]"
showType (TVar _ x)  = "typeof(" ++ x ++ ")"

showPos :: Position -> String
showPos (l, c) = "(line: " ++ show l ++ ", col: " ++ show c ++ ")"

makeId :: Ident -> String
makeId (Ident _ x)  = x
makeId (Select x y) = (makeId x) ++ "." ++ (makeId y)

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
