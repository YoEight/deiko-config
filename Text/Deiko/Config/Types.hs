{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances     #-}
module Text.Deiko.Config.Types where

import Control.Exception          (ioError)
import Control.Monad.Cont         (ContT)
import Control.Monad.Reader       (ReaderT)
import Control.Monad.State        (StateT)
import Control.Monad.Trans        (lift)

import Data.Char                  (isDigit)
import Data.String                (IsString (..))

import System.IO.Error            (userError)

import Text.Deiko.Config.Semantic

newtype ConfigError = ConfigError String deriving Show

class Monad m => CanReport m where
  configError :: ConfigError -> m a

class ConfigValue v where
  configValue :: CanReport m => String -> PropValue -> m v

instance ConfigValue PropValue where
  configValue _ = return

instance ConfigValue String where
  configValue _ (PSTRING x) = return $ fromString x
  configValue key  x = reportError $ expErrMsg key "String" (showType x)

instance ConfigValue Integer where
  configValue _ (PSTRING xs)
    | all isDigit xs = return $ read xs
    | otherwise      = reportError $ ("Error when parsing an Integer: value ["
                                      ++ xs ++ "] is not a Integer")
  configValue key x = reportError $ expErrMsg key "Integer" (showType x)

instance ConfigValue Bool where
  configValue _ (PSTRING xs) = makeBool xs
    where
      makeBool "True"  = return True
      makeBool "true"  = return True
      makeBool "Yes"   = return True
      makeBool "yes"   = return True
      makeBool "False" = return False
      makeBool "false" = return False
      makeBool "No"    = return False
      makeBool "no"    = return False
      makeBool x       = reportError ("Error when parsing a Bool: value ["
                                     ++ x ++ "] is not a Bool")
  configValue key x    = reportError $ expErrMsg key "Bool" (showType x)

instance CanReport IO where
    configError (ConfigError msg) = ioError $ userError msg

instance CanReport m => CanReport (ReaderT r m) where
  configError = lift . configError

instance CanReport m => CanReport (StateT r m) where
  configError = lift . configError

instance CanReport m => CanReport (ContT r m) where
  configError = lift . configError

reportError :: CanReport m => String -> m a
reportError = configError . ConfigError

showType (PSTRING _) = "String"
showType (PLIST _)   = "List"
showType (POBJECT _) = "Object"

substErrMsg key subst =
  "Error when retrieving [" ++ key ++ "]: " ++
  "Subst. key [" ++ subst ++ "] not found"

propErrMsg key =
  "Property [" ++ key ++ "] not found"

expErrMsg key expected found =
  "Error when parsing [" ++ key ++ "]: " ++
  "Expecting: " ++ expected ++ " Found: " ++ found
