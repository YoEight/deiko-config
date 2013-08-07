{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
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
import Data.Functor.Identity (Identity)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Hashable
import Data.Monoid (Monoid)
import Data.String (IsString (..))
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic
import Text.Deiko.Config.Util (Mu(..), cata)
import Text.Deiko.Config.Types
import qualified Text.Deiko.Config.Internal as I
import Text.Parsec.Prim (Stream)

data CString = CString
data CList a = CList a
data CObject = CObject
data Val s a b = Val (I.Value s b)
data Pair s = forall t. TypeOf t => Pair s (Val s t (s, Type s))

class Uni v

class TypeOf t where
  typeof :: IsString s => Val s t a -> Type s

instance TypeOf CString where
  typeof _ = I.stringType

instance TypeOf CObject where
  typeof _ = I.objectType

instance TypeOf t => TypeOf (CList t) where
  typeof _ = I.listTypeOf (typeof (undefined :: IsString s => Val s t a))

instance Uni CString
instance Uni v => Uni (CList v)
instance Uni CObject

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

prop :: (TypeOf t, IsString s) => s -> Val s t (s, Type s) -> Pair s
prop name value = Pair name value

object :: IsString s => [Pair s] -> Val s CObject (s, Type s)
object = Val . I.object falsePos . fmap go
  where
    go (Pair name v@(Val value)) = I.property (name, typeof v) value

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: (Functor m, Monad m, I.StringLike s)
        => Conduit Char (ErrorT (I.ConfigError s) m) (Config s)
compile = lexer =$= parser =$= typecheck

loadFile :: (MonadResource m, I.StringLike s)
         => String
         -> Source (ErrorT (I.ConfigError s) m) (Config s)
loadFile path = sourceFile path $= bytesToChar =$= compile

printout :: (MonadIO m, Show a) => Sink a m ()
printout = awaitForever (liftIO . print)

-- testing
file :: String -> ErrorT (I.ConfigError String) (ResourceT IO) (Config String)
file p = loadFile p $$ ((maybe (error "") return) =<< await)
