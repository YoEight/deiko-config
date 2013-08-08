{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE OverloadedStrings         #-}
module Text.Deiko.Config
  ( module Text.Deiko.Config.Core
  , bytesToChar
  , sourceString
  , compile
  , loadFile
  ) where

import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.Resource (MonadResource)
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
import Text.Deiko.Config.Internal (StringLike, ConfigError)

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: (Functor m, Monad m, StringLike s)
        => Conduit Char (ErrorT (ConfigError s) m) (Config s)
compile = lexer =$= parser =$= typecheck

loadFile :: (MonadResource m, StringLike s)
         => String
         -> Source (ErrorT (ConfigError s) m) (Config s)
loadFile path = sourceFile path $= bytesToChar =$= compile
