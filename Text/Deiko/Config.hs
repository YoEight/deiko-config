module Text.Deiko.Config (module Text.Deiko.Config.Core
                         ,ConfigValue(..)
                         ,CanReport(..)
                         ,loadConfig) where

import Control.Monad.Trans        (MonadIO (..))

import Text.Deiko.Config.Core
import Text.Deiko.Config.Semantic
import Text.Deiko.Config.Types 

loadConfig :: (CanReport m, MonadIO m) => String -> m Config
loadConfig path =
  do file <- liftIO $ readFile path
     either reportError return (compile file)
