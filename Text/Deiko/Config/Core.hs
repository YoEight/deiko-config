{-# LANGUAGE DeriveDataTypeable #-}
module Text.Deiko.Config.Core () where

import           Control.Exception
import           Control.Monad              ((<=<))
import           Control.Monad.Reader
import           Control.Monad.Trans

import qualified Data.Map                   as M
import           Data.Typeable

import           Text.Deiko.Config.Semantic

data ConfigError = ConfigError String
  deriving (Show, Typeable)

instance Exception ConfigError

class HasConfig a where
  getConfig :: a -> Config

loadConfig :: MonadIO m => String -> m Config
loadConfig =
  liftIO . (either (throwIO . ConfigError) return . compile <=< readFile)

getString :: (HasConfig r, MonadReader r m) => String -> m (Maybe String)
getString key =
  do register <- asks (configRegister . getConfig)
     return $ (M.lookup key register) >>= go
  where
    go (PSTRING x) = Just x
    go _           = Nothing
