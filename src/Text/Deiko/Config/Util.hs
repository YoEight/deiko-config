{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Deiko.Config.Util where

import Control.Monad.Free (Free(..))

data Cons a b = Cons a b
              | Nil

instance Functor (Cons a) where
  fmap f (Cons a b) = Cons a (f b)
  fmap _ Nil        = Nil

instance (Show a, Show b) => Show (Cons a b) where
  show (Cons a b) = "Cons (" ++ show a ++ ", " ++ show b ++ ")"
  show Nil        = "Nil"

newtype Mu f = Mu (f (Mu f))

instance Show (f (Mu f)) => Show (Mu f) where
  show = show . out

out :: Mu f -> f (Mu f)
out (Mu x) = x

cata :: Functor f => (f a -> a) -> Mu f -> a
cata k = k . fmap (cata k) . out

para :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
para k x@(Mu m) = k $ fmap (\m' -> (x, para k m')) m

cataList :: (Cons a b -> b) -> [a] -> b
cataList k [] = k Nil
cataList k (x:xs) = k (Cons x (cataList k xs))

cataFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
cataFree k _ (Pure a) = k a
cataFree p k (Free f) = k (fmap (cataFree p k) f)
