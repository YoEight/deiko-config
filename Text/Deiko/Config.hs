{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Deiko.Config where

import Control.Monad.Error
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Conduit, Producer, Sink, Source, ($=), ($$), (=$=), yield
                    ,awaitForever, runResourceT, ResourceT, await)
import Data.Conduit.Binary (sourceFile)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Hashable
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic
import Text.Deiko.Config.Util (Mu(..), cata)
import Text.Deiko.Config.Types
import qualified Text.Deiko.Config.Internal as I

data CString = CString
data CList a = CList a
data CObject = CObject
data Val a b = Val (I.Value b)
data Pair = forall t. TypeOf t => Pair String (Val t (String, Type))

class Uni v

class ToParams v where
  toParams :: v -> [Pair]

class TypeOf t where
  typeof :: Val t a -> Type

instance TypeOf CString where
  typeof _ = I.stringType

instance TypeOf CObject where
  typeof _ = I.objectType

instance TypeOf t => TypeOf (CList t) where
  typeof _ = I.listTypeOf (typeof (undefined :: Val t a))

instance TypeOf v => ToParams (String, Val v (String, Type)) where
  toParams (key, v) = [prop key v]

instance ToParams [Pair] where
  toParams = id

instance Uni CString
instance Uni v => Uni (CList v)
instance Uni CObject

getValue :: (Monad m, ConfigValue v)
         => String
         -> Config
         -> ErrorT I.ConfigError m v
getValue key (Config reg st) = maybe (throwError $ propertyNotFound key) go (IM.lookup (hash key) reg)
  where
    go (typ, value) = runReaderT (configValue key typ value) (tsTable st)

falsePos :: I.Position
falsePos = (-1, -1)

string :: String -> Val CString a
string = Val . I.string falsePos

list :: [Val a b] -> Val (CList a) b
list = Val . I.list falsePos . fmap (\(Val x) -> x)

nil :: Val (CList a) b
nil = Val $ I.nil falsePos

merge :: Val a b -> Val a b -> Val a b
merge (Val x) (Val y) = Val $ I.merge x y

prop :: TypeOf t => String -> Val t (String, Type) -> Pair
prop name value = Pair name value

object :: [Pair] -> Val CObject (String, Type)
object = Val . I.object falsePos . fmap go
  where
    go (Pair name v@(Val value)) = I.property (name, typeof v) value

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: (Functor m, Monad m)
        => Conduit Char (ErrorT I.ConfigError m) Config
compile = lexer =$= parser =$= typecheck

loadFile :: MonadResource m
         => String
         -> Source (ErrorT I.ConfigError m) Config
loadFile path = sourceFile path $= bytesToChar =$= compile

printout :: (MonadIO m, Show a) => Sink a m ()
printout = awaitForever (liftIO . print)

-- testing
file :: String -> ErrorT I.ConfigError (ResourceT IO) Config
file p = loadFile p $$ ((maybe (error "") return) =<< await)
