{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Text.Deiko.Config where

import Control.Monad.Error
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Conduit, Producer, Sink, Source, ($=), ($$), (=$=), yield
                    ,awaitForever, runResourceT, ResourceT, await)
import Data.Conduit.Binary (sourceFile)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic
import Text.Deiko.Config.Util (Mu(..))
import Text.Deiko.Config.Types
import qualified Text.Deiko.Config.Internal as I

data CString = CString
data CList a = CList a
data CObject = CObject
data Val a b = Val (I.Value b)
data Pair = forall t. Uni t => Pair String (Val t String)

class Uni v

instance Uni CString
instance Uni v => Uni (CList v)
instance Uni CObject

getValue :: (Monad m, ConfigValue v) 
         => String
         -> Register
         -> ErrorT I.ConfigError m v
getValue key = maybe (throwError $ propertyNotFound key) go . M.lookup key
  where
    go (typ, value) = configValue key typ value

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

prop :: Uni t => String -> Val t String -> Pair
prop name value = Pair name value

object :: [Pair] -> Val CObject String
object = Val . I.object falsePos . fmap go
  where
    go (Pair name (Val value)) = I.property name value

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: (Functor m, Monad m) 
        => Conduit Char (ErrorT I.ConfigError m) Register
compile = lexer =$= parser =$= typecheck

loadFile :: MonadResource m
         => String
         -> Source (ErrorT I.ConfigError m) Register
loadFile path = sourceFile path $= bytesToChar =$= compile

printout :: (MonadIO m, Show a) => Sink a m ()
printout = awaitForever (liftIO . print)

-- testing
file :: String -> ErrorT I.ConfigError (ResourceT IO) Register
file p = loadFile p $$ ((maybe (error "") return) =<< await) 
