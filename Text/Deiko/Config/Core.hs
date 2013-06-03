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
import           Control.Monad.Error        (MonadError (..))
import           Control.Monad.Reader       (MonadReader, asks, runReaderT)
import           Control.Monad.State        (execStateT, get, put)
import           Control.Monad.Trans        (MonadIO (..), lift)
import           Control.Monad.Trans.Either (runEitherT)

import           Data.Foldable              (traverse_)
import           Data.List                  (unionBy)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Data.Traversable           (traverse)

import           Text.Deiko.Config.Semantic

newtype ConfigError = ConfigError String deriving Show

class HasConfig a where
  getConfig :: a -> Config

class CanReport r where
  configError :: ConfigError -> r

class ConfigValue v where
  configValue :: (CanReport e, MonadError e m) => String -> PropValue -> m v

instance ConfigValue PropValue where
  configValue _ = return

instance ConfigValue String where
  configValue _ (PSTRING x) = return $ fromString x
  configValue key  x = reportError $ expErrMsg key "String" (showType x)

instance HasConfig Config where
  getConfig = id

instance CanReport ConfigError where
  configError = id

loadConfig :: (CanReport e, MonadIO m, MonadError e m) => String -> m Config
loadConfig path =
  do file <- liftIO $ readFile path
     either (throwError . configError . ConfigError) return (compile file)

getValue :: (ConfigValue v, CanReport e, HasConfig r, MonadReader r m
            , MonadError e m)
         => String
         -> m v
getValue key =
  getEnv (transform configValue defaultSubstHandler key)
           (reportError . propErrMsg) key

getValues :: (ConfigValue v, CanReport e, HasConfig r, MonadReader r m
             , MonadError e m)
          => String
          -> m [v]
getValues key =
  getEnv (transform go defaultSubstHandler key)
           (reportError . propErrMsg) key
  where
    go key (PLIST xs) =
      unwrapMonad (traverse (WrapMonad . configValue key) xs)
    go key x = reportError $ expErrMsg key "List" (showType x)

getEnv :: (HasConfig r, CanReport e, MonadReader r m, MonadError e m)
       => (Register -> PropValue -> m v)
       -> (String -> m v)
       -> String
       -> m v
getEnv onSuccess onError key =
  do register <- asks (configRegister . getConfig)
     maybe (onError key) (onSuccess register) (M.lookup key register)

transform :: (CanReport e, Monad m, MonadError e m)
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
transform f _ key _ x = f key x

defaultSubstHandler :: (CanReport e, MonadError e m) => String -> String -> m a
defaultSubstHandler key sub = reportError $ substErrMsg key sub

reportError :: (CanReport e, MonadError e m) => String -> m a
reportError = throwError . configError . ConfigError

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
