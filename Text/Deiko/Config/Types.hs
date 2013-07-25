{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Deiko.Config.Types where

import Control.Monad.Error
import Data.Char (isDigit)
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

class ConfigValue v where
  configValue :: Monad m
              => String
              -> Type
              -> Mu (AST (String, Type))
              -> ErrorT ConfigError m v

instance ConfigValue String where
  configValue _ (TString _) value = cata (\(ASTRING _ s) -> return s) value
  configValue key typ _           = throwError $ wrongType key stringType typ

instance ConfigValue Int where
  configValue key (TString _) value = cata go value
    where
      go (ASTRING _ s)
         | all isDigit s = return $ read s
         | otherwise     = throwError $ parseError key s "Int"
  configValue key typ _ = throwError $ parseTypeError key "Int" typ

propertyNotFound :: String -> ConfigError
propertyNotFound prop = ConfigError ("Property [" ++ prop ++ "] not found")

wrongType :: String -> Type -> Type -> ConfigError
wrongType key expected found = ConfigError msg
  where
    expStr = showType expected
    fouStr = showType found
    msg = "Type error on [" ++ key ++ "]. expected: "
          ++ expStr ++ ", found: " ++ fouStr

parseError :: String -> String -> String -> ConfigError
parseError key val typ = ConfigError msg
  where
    msg = "Can't parse a " ++ typ ++ " for [" ++ key ++ "] with value ["
          ++ val ++ "]"

parseTypeError :: String -> String -> Type -> ConfigError
parseTypeError key expected found = ConfigError msg
  where
    fouStr = showType found
    msg = "Can't parse a " ++ expected ++ " with a value of type " ++ fouStr
          ++ " for [" ++ key ++ "]"

stringType :: Type
stringType = TString (-1, -1)
