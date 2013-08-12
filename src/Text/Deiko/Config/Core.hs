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
import Data.Conduit (Conduit, Producer, Sink, Source, ($=), ($$), (=$=), yield
                    ,awaitForever, runResourceT, ResourceT, await)
import Data.Functor.Identity (Identity)
import Data.Foldable (traverse_)
import qualified Data.IntMap as IM
import Data.Hashable (hash)
import Data.String (IsString (..))
import qualified Text.Deiko.Config.Internal as I
import Text.Deiko.Config.Semantic (Config (..), TypeState (..), typecheck
                                  ,manualSimplify)
import Text.Deiko.Config.Types (stringValue, intValue, boolValue, listValue
                               ,propertyNotFound, Conversion)
import Text.Parsec.Prim (Stream)

data CString = CString
data CList a = CList a
data CObject = CObject
data Val s a b = Val (I.Value s b)
data Pair s = forall t. TypeOf t => s := (Val s t (s, I.Type s))

class TypeOf t where
  typeof :: IsString s => Val s t a -> I.Type s

instance TypeOf CString where
  typeof _ = I.stringType

instance TypeOf CObject where
  typeof _ = I.objectType

instance TypeOf t => TypeOf (CList t) where
  typeof _ = I.listTypeOf (typeof (undefined :: IsString s => Val s t a))

falsePos :: I.Position
falsePos = (-1, -1)

string :: IsString s => s -> Val s CString a
string = Val . I.string falsePos

list :: [Val s a b] -> Val s (CList a) b
list = Val . I.list falsePos . fmap (\(Val x) -> x)

nil :: Val s (CList a) b
nil = Val $ I.nil falsePos

merge :: Val s a b -> Val s a b -> Val s a b
merge (Val x) (Val y) = Val $ I.merge x y

object :: IsString s => [Pair s] -> Val s CObject (s, I.Type s)
object = Val . I.object falsePos . fmap go
  where
    go (name := v@(Val value)) = I.property (name, typeof v) value

getValueWithAs :: (I.StringLike s, Monad m, Functor m)
               => Conversion s v
               -> [Pair s]
               -> s
               -> Config s
               -> ErrorT (I.ConfigError s) m v
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

getValueAs :: (I.StringLike s, Monad m)
           => Conversion s v
           -> s
           -> Config s
           -> ErrorT (I.ConfigError s) m v
getValueAs k key (Config reg st) =
  maybe (throwError $ propertyNotFound key) go (IM.lookup (hash key) reg)
    where
      go (typ, value) = runReaderT (k key typ value) (tsTable st)

getListOf :: (I.StringLike s, Monad m)
          => Conversion s v
          -> s
          -> Config s
          -> ErrorT (I.ConfigError s) m [v]
getListOf k = getValueAs (listValue k)

getString :: (I.StringLike s, Monad m)
          => s
          -> Config s
          -> ErrorT (I.ConfigError s) m s
getString = getValueAs stringValue

getInt :: (I.StringLike s, Stream s Identity Char, Monad m)
       => s
       -> Config s
       -> ErrorT (I.ConfigError s) m Int
getInt = getValueAs intValue

getBool :: (I.StringLike s, Stream s Identity Char, Monad m)
        => s
        -> Config s
        -> ErrorT (I.ConfigError s) m Bool
getBool = getValueAs boolValue

getStrings :: (I.StringLike s, Monad m)
           => s
           -> Config s
           -> ErrorT (I.ConfigError s) m [s]
getStrings = getListOf stringValue

getInts :: (I.StringLike s, Stream s Identity Char, Monad m)
        => s
        -> Config s
        -> ErrorT (I.ConfigError s) m [Int]
getInts = getListOf intValue

getBools :: (I.StringLike s, Stream s Identity Char, Monad m)
         => s
         -> Config s
         -> ErrorT (I.ConfigError s) m [Bool]
getBools = getListOf boolValue

getStringWith :: (I.StringLike s, Monad m, Functor m)
              => [Pair s]
              -> s
              -> Config s
              -> ErrorT (I.ConfigError s) m s
getStringWith = getValueWithAs stringValue

getIntWith :: (I.StringLike s, Stream s Identity Char, Monad m, Functor m)
              => [Pair s]
              -> s
              -> Config s
              -> ErrorT (I.ConfigError s) m Int
getIntWith = getValueWithAs intValue

getBoolWith :: (I.StringLike s, Stream s Identity Char, Monad m, Functor m)
            => [Pair s]
            -> s
            -> Config s
            -> ErrorT (I.ConfigError s) m Bool
getBoolWith = getValueWithAs boolValue
