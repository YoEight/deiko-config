{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE OverloadedStrings         #-}
module Text.Deiko.Config
  ( module Text.Deiko.Config.Core
  , bytesToChar
  , configMsg
  , sourceString
  , compile
  , loadFile
  , printFileTokens
  ) where

import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Error (ErrorT, mapErrorT)
import Data.Conduit (Conduit, Sink, Source, Producer, ($=), ($$), (=$=), yield
                    ,await, awaitForever, runResourceT, ResourceT)
import Data.Conduit.Binary (sourceFile)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic (Config, typecheck)
import Text.Deiko.Config.Core
import Text.Deiko.Config.Types
import Text.Deiko.Config.Internal (StringLike, ConfigError (..))

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: (Functor m, Monad m, StringLike s)
        => Conduit Char (ErrorT (ConfigError s) m) (Config s)
compile = lexer =$= parser =$= typecheck

loadFile ::  StringLike s => String -> ErrorT (ConfigError s) IO (Config s)
loadFile path =
  mapErrorT runResourceT
              (sourceFile path $= bytesToChar =$= compile $$ (await >>= go))
  where
    go (Just x) = return x

printFileTokens :: StringLike s => String -> ErrorT (ConfigError s) IO ()
printFileTokens path = mapErrorT runResourceT
                  (sourceFile path $= bytesToChar =$= lexer $$ printOut)

configMsg :: ConfigError s -> s
configMsg (ConfigError s) = s

printOut :: (MonadIO m, Show a) => Sink a m ()
printOut = awaitForever (liftIO . print)
