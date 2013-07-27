{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
module Text.Deiko.Config.Types where

import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader (MonadReader)
import Data.Char (isDigit)
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

class ConfigValue v where
  configValue :: (MonadReader TypeTable m, MonadError ConfigError m)
              => String
              -> Type
              -> Mu (AST (String, Type))
              -> m v

instance ConfigValue String where
  configValue key typ value =
    sameType typ stringType >>= \same ->
      case () of
        _ | same      -> cata (\(ASTRING _ s) -> return s) value
          | otherwise ->
            showType typ >>= \typStr ->
              throwError $ wrongType key "String" typStr

-- instance ConfigValue Int where
--   configValue key (TString _) value = cata go value
--     where
--       go (ASTRING _ s)
--          | all isDigit s = return $ read s
--          | otherwise     = throwError $ parseError key s "Int"
--   configValue key typ _ = throwError $ parseTypeError key "Int" typ

propertyNotFound :: String -> ConfigError
propertyNotFound prop = ConfigError ("Property [" ++ prop ++ "] not found")

wrongType :: String -> String -> String -> ConfigError
wrongType key expected found = ConfigError msg
  where
    msg = "Type error on [" ++ key ++ "]. expected: "
          ++ expected ++ ", found: " ++ found

-- parseError :: String -> String -> String -> ConfigError
-- parseError key val typ = ConfigError msg
--   where
--     msg = "Can't parse a " ++ typ ++ " for [" ++ key ++ "] with value ["
--           ++ val ++ "]"

-- parseTypeError :: String -> String -> Type -> ConfigError
-- parseTypeError key expected found = ConfigError msg
--   where
--     fouStr = showType found
--     msg = "Can't parse a " ++ expected ++ " with a value of type " ++ fouStr
--           ++ " for [" ++ key ++ "]"
