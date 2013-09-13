{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE GADTs                     #-}
module Text.Deiko.Config
  ( module Text.Deiko.Config.Core
  , bytesToChar
  , configMsg
  , sourceString
  , compile
  , loadFile
  ) where

import Control.Monad (forever, unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (MonadIO (..), lift)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic (Config, typecheck)
import Text.Deiko.Config.Core
import Text.Deiko.Config.Types
import Text.Deiko.Config.Internal (ConfigError (..))
import Pipes (Pipe, Producer, Producer', yield, await, runEffect, (>->), for, cat)
import Pipes.Safe (runSafeT)
import Pipes.Safe.Prelude (withFile)
import System.IO (Handle, IOMode(..))

bytesToChar :: Monad m => Pipe (Maybe ByteString) (Maybe Char) m r
bytesToChar = for cat go
  where
    go =
      maybe (yield Nothing) (traverse_ (yield . Just) . unpack)

stringToChar :: Monad m => Pipe (Maybe String) (Maybe Char) m r
stringToChar = for cat go
  where
    go =
      maybe (yield Nothing) (traverse_ (yield . Just))

sourceString :: Monad m => String -> Producer (Maybe Char) m r
sourceString xs = do
  traverse_ (yield . Just) xs
  yield Nothing
  sourceString xs

compile :: (Functor m, MonadCatch m) => Pipe (Maybe Char) Config m r
compile = lexer >-> parser >-> typecheck

loadFile :: String -> IO Config
loadFile path = runSafeT $ runEffect $ withFile path ReadMode effect
  where
    effect h =
      sourceFileHandle h >-> bytesToChar >-> compile >-> (await >>= return)

sourceFileHandle :: MonadIO m => Handle -> Producer (Maybe ByteString) m r
sourceFileHandle h = do
  loop
  forever $ yield Nothing
    where
      loop = do
        bytes <- liftIO $ B.hGetSome h 8192
        unless (B.null bytes) $ do
                       yield $ Just bytes
                       loop

configMsg :: ConfigError -> T.Text
configMsg (ConfigError s) = s
