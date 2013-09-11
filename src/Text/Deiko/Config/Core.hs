{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Text.Deiko.Config.Core
  ( string
  , list
  , nil
  , merge
  , object
  , getValueAs
  , getValueWithAs
  , getListOf
  , getString
  , getInt
  , getBool
  , getStrings
  , getInts
  , getBools
  , getStringWith
  , getIntWith
  , getBoolWith
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Error (ErrorT, throwError)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (get, put, execState)
import Data.Conduit (($$), await)
import Data.Foldable (traverse_)
import qualified Data.IntMap as IM
import Data.Hashable (hash)
import qualified Data.Text as T
import qualified Text.Deiko.Config.Internal as I
import Text.Deiko.Config.Semantic (Config (..), TypeState (..), typecheck
                                  ,manualSimplify)
import Text.Deiko.Config.Types (stringValue, intValue, boolValue, listValue
                               ,propertyNotFound, Conversion)

data CString = CString
data CList a = CList a
data CObject = CObject
data Val a b = Val (I.Value b)
data Pair = forall t. TypeOf t => T.Text := (Val t (T.Text, I.Type))

class TypeOf t where
  typeof ::  Val t a -> I.Type

instance TypeOf CString where
  typeof _ = I.stringType

instance TypeOf CObject where
  typeof _ = I.objectType

instance TypeOf t => TypeOf (CList t) where
  typeof _ = I.listTypeOf (typeof (undefined :: Val t a))

falsePos :: I.Position
falsePos = (-1, -1)

string :: T.Text -> Val CString a
string = Val . I.string falsePos

list :: [Val a b] -> Val (CList a) b
list = Val . I.list falsePos . fmap (\(Val x) -> x)

nil :: Val (CList a) b
nil = Val $ I.nil falsePos

merge :: Val a b -> Val a b -> Val a b
merge (Val x) (Val y) = Val $ I.merge x y

object :: [Pair] -> Val CObject (T.Text, I.Type)
object = Val . I.object falsePos . fmap go
  where
    go (name := v@(Val value)) = I.property (name, typeof v) value

getValueWithAs :: ( Monad m, Functor m)
               => Conversion v
               -> [Pair]
               -> T.Text
               -> Config
               -> ErrorT I.ConfigError m v
getValueWithAs k [] key config = getValueAs k key config
getValueWithAs k xs key config =
  process $$ (await >>= (lift . getValueAs k key . unJust))
  where
    unJust (Just x) = x
    toProp (key := v@(Val value)) = (hash key, (typeof v, value))
    props   = fmap toProp xs
    go (hash, tup) =
      get >>= \(Config reg (TypeState tb cs)) ->
        let reg1 = IM.insertWith (flip const) hash tup reg
            tb1  = IM.insertWith (flip const) hash (fst tup) tb in
        put (Config reg1 (TypeState tb1 cs))
    process =
      let config1 = execState (traverse_ go props) config in
      manualSimplify config1

getValueAs :: Monad m
           => Conversion v
           -> T.Text
           -> Config
           -> ErrorT I.ConfigError m v
getValueAs k key (Config reg st) =
  maybe (throwError $ propertyNotFound key) go (IM.lookup (hash key) reg)
    where
      go (typ, value) = runReaderT (k key typ value) (tsTable st)

getListOf :: Monad m
          => Conversion v
          -> T.Text
          -> Config
          -> ErrorT I.ConfigError m [v]
getListOf k = getValueAs (listValue k)

getString :: Monad m
          => T.Text
          -> Config
          -> ErrorT I.ConfigError m T.Text
getString = getValueAs stringValue

getInt :: Monad m
       => T.Text
       -> Config
       -> ErrorT I.ConfigError m Int
getInt = getValueAs intValue

getBool :: Monad m
        => T.Text
        -> Config
        -> ErrorT I.ConfigError m Bool
getBool = getValueAs boolValue

getStrings :: Monad m
           => T.Text
           -> Config
           -> ErrorT I.ConfigError m [T.Text]
getStrings = getListOf stringValue

getInts :: Monad m
        => T.Text
        -> Config
        -> ErrorT I.ConfigError m [Int]
getInts = getListOf intValue

getBools :: Monad m
         => T.Text
         -> Config
         -> ErrorT I.ConfigError m [Bool]
getBools = getListOf boolValue

getStringWith :: (Monad m, Functor m)
              => [Pair]
              -> T.Text
              -> Config
              -> ErrorT I.ConfigError m T.Text
getStringWith = getValueWithAs stringValue

getIntWith :: (Monad m, Functor m)
              => [Pair]
              -> T.Text
              -> Config
              -> ErrorT I.ConfigError m Int
getIntWith = getValueWithAs intValue

getBoolWith :: (Monad m, Functor m)
            => [Pair]
            -> T.Text
            -> Config
            -> ErrorT I.ConfigError m Bool
getBoolWith = getValueWithAs boolValue
