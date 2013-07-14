module Text.Deiko.Config.Util where

newtype Mu f = Mu (f (Mu f))

data Free f a = Return a
              | Suspend (f (Free f a))

instance Functor f => Monad (Free f) where
  return = Return

  Return a  >>= f = f a
  Suspend m >>= f = Suspend $ fmap (f =<<) m

out :: Mu f -> f (Mu f)
out (Mu x) = x

cata :: Functor f => (f a -> a) -> Mu f -> a
cata k = k . fmap (cata k) . out

cataFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
cataFree k _ (Return a) = k a
cataFree p k (Suspend f) = k (fmap (cataFree p k) f)
