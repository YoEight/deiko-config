{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.Deiko.Config.Types where

import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader (MonadReader)
import Data.Char (isDigit)
import Data.Hashable
import Data.Monoid (Monoid, (<>))
import Data.String (IsString)
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

class ( IsString s
      , Monoid s
      , Hashable s
      , MonadReader (TypeTable s) m
      , MonadError (ConfigError s) m) => ConfigCtx s m

-- class ConfigValue v where
--   configValue :: ConfigCtx s m
--               => s
--               -> Type s
--               -> Mu (AST s (s, Type s))
--               -> m v

class ConfigValue p t f m v | p -> v, t -> v, f -> v, m -> v where
  configValue :: p -> t -> f -> m v

instance (ConfigCtx s m) => ConfigValue s (Type s) (Annoted s) m s where
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

propertyNotFound :: (IsString s, Monoid s) => s -> ConfigError s
propertyNotFound prop = ConfigError ("Property [" <> prop <> "] not found")

wrongType :: (IsString s, Monoid s) => s -> s -> s -> ConfigError s
wrongType key expected found = ConfigError msg
  where
    msg = "Type error on [" <> key <> "]. expected: "
          <> expected <> ", found: " <> found

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
