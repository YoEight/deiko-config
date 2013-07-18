{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Deiko.Config.Util where

import Control.Monad.Free (Free(..))

newtype Mu f = Mu (f (Mu f))

instance Show (f (Mu f)) => Show (Mu f) where
  show = show . out

out :: Mu f -> f (Mu f)
out (Mu x) = x

cata :: Functor f => (f a -> a) -> Mu f -> a
cata k = k . fmap (cata k) . out

cataFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
cataFree k _ (Pure a) = k a
cataFree p k (Free f) = k (fmap (cataFree p k) f)
