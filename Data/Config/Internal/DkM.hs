{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.DkM
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.DkM where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

--------------------------------------------------------------------------------
newtype DkM e s a = DkM { runDkM :: e -> s -> IO (a, s) }

--------------------------------------------------------------------------------
-- | Instances
--------------------------------------------------------------------------------
instance Functor (DkM e s) where
    fmap f (DkM k)
        = DkM $ \e s -> fmap (\(a, s') -> (f a, s')) (k e s)

--------------------------------------------------------------------------------
instance Applicative (DkM e s) where
    pure  = return
    (<*>) = ap

--------------------------------------------------------------------------------
instance Monad (DkM e s) where
    return a = DkM $ \_ s -> return (a, s)

    DkM k >>= f
        = DkM $ \e s -> do
            (a, !s') <- k e s
            runDkM (f a) e s'

--------------------------------------------------------------------------------
instance MonadState s (DkM e s) where
    state k = DkM $ \_ s -> return $ k s

--------------------------------------------------------------------------------
instance MonadReader e (DkM e s) where
    ask = DkM $ \e s -> return (e, s)

    local f (DkM k) = DkM $ \e s -> k (f e) s

--------------------------------------------------------------------------------
instance MonadIO (DkM e s) where
    liftIO m = DkM $ \_ s -> fmap (\a -> (a, s)) m

--------------------------------------------------------------------------------
execDkM :: DkM e s a -> e -> s -> IO s
execDkM m e s = fmap snd (runDkM m e s)

--------------------------------------------------------------------------------
evalDkM :: DkM e s a -> e -> s -> IO a
evalDkM m e s = fmap fst (runDkM m e s)
