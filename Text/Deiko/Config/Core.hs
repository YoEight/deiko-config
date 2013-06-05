{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Text.Deiko.Config.Core (loadConfig
                              ,getValue
                              ,getValues
                              ,HasConfig(..)
                              ,CanReport(..)
                              ,ConfigError(..)) where

import           Control.Applicative        (WrappedMonad (..))
import           Control.Monad              (liftM)
import           Control.Monad.Reader       (MonadReader, asks, runReaderT)
import           Control.Monad.State        (execStateT, get, put)
import           Control.Monad.Trans        (MonadIO (..), lift)

import           Data.Char                  (isDigit)
import           Data.Foldable              (traverse_)
import           Data.List                  (unionBy)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Data.Traversable           (traverse)

import           Text.Deiko.Config.Semantic

newtype ConfigError = ConfigError String deriving Show

class HasConfig a where
  getConfig :: a -> Config

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
  configValue key x = reportError $ expErrMsg key "Int" (showType x)

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

instance HasConfig Config where
  getConfig = id

loadConfig :: (CanReport m, MonadIO m) => String -> m Config
loadConfig path =
  do file <- liftIO $ readFile path
     either reportError return (compile file)

getValue :: (ConfigValue v, CanReport m, HasConfig r, MonadReader r m)
         => String
         -> m v
getValue key =
  getEnv (transform configValue defaultSubstHandler key)
           (reportError . propErrMsg) key

getValues :: (ConfigValue v, CanReport m, HasConfig r, MonadReader r m)
          => String
          -> m [v]
getValues key =
  getEnv (transform go defaultSubstHandler key)
           (reportError . propErrMsg) key
  where
    go key (PLIST xs) =
      unwrapMonad (traverse (WrapMonad . configValue key) xs)
    go key x = reportError $ expErrMsg key "List" (showType x)

getEnv :: (HasConfig r, CanReport m, MonadReader r m)
       => (Register -> PropValue -> m v)
       -> (String -> m v)
       -> String
       -> m v
getEnv onSuccess onError key =
  do register <- asks (configRegister . getConfig)
     maybe (onError key) (onSuccess register) (M.lookup key register)

transform :: CanReport m
          => (String -> PropValue -> m v)
          -> (String -> String -> m v) -- on susbstitution
          -> String
          -> Register
          -> PropValue
          -> m v
transform f onsub key reg (PSUBST sub) =
  case sub of
    ('?':skey) -> maybe (transform f onsub key reg (PSTRING ""))
                  (transform f onsub key reg)
                  (M.lookup skey reg)
    _          -> maybe (onsub key sub)
                  (transform f onsub key reg)
                  (M.lookup sub reg)
transform f onsub key reg (PCONCAT (x:xs)) =
  transform f onsub key reg =<< action
    where
      action = execStateT (unwrapMonad $ traverse_ (WrapMonad . concat) xs) x

      concat v =
        do acc <- get
           v1  <- lift $ transform (const return) defaultSubstHandler key reg v
           put (merge acc v1)

      merge (PSTRING x) (PSTRING y)   = PSTRING (x ++ " " ++ y)
      merge (PLIST xs) (PLIST ys)     = PLIST (xs ++ ys)
      merge (POBJECT (Object xs)) (POBJECT (Object ys)) =
        let f x y = (str $ propName x) == (str $ propName y)
            str (PSTRING x) = x in
        POBJECT (Object $ unionBy f ys xs)
transform f onsub key reg (PLIST xs) =
  f key =<< action
  where
    action = unwrapMonad $ fmap PLIST (traverse go xs)

    go = WrapMonad . transform constM defaultSubstHandler key reg

    constM = const return
transform f _ key _ x = f key x

defaultSubstHandler :: CanReport m => String -> String -> m a
defaultSubstHandler key sub = reportError $ substErrMsg key sub

reportError :: CanReport m => String -> m a
reportError = configError . ConfigError

showType (PSTRING _) = "String"
showType (PLIST _)   = "List"
showType (POBJECT _) = "Object"
showType (PSUBST x)  = "*substitution* [" ++ x  ++ "]"

substErrMsg key subst =
  "Error when retrieving [" ++ key ++ "]: " ++
  "Subst. key [" ++ subst ++ "] not found"

propErrMsg key =
  "Property [" ++ key ++ "] not found"

expErrMsg key expected found =
  "Error when parsing [" ++ key ++ "]: " ++
  "Expecting: " ++ expected ++ " Found: " ++ found
