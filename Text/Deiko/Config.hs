{-# LANGUAGE RankNTypes #-}
module Text.Deiko.Config where

import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (Conduit, Producer, Sink, Source, ($=), ($$), (=$=), yield
                    ,awaitForever, runResourceT)
import Data.Conduit.Binary (sourceFile)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (parser)
import Text.Deiko.Config.Semantic
import Text.Deiko.Config.Util (Mu(..)) 
import Text.Deiko.Config.Internal (AST(..), Prop, Position)

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

compile :: Monad m => Conduit Char m (Either String Register)
compile = lexer =$= parser =$= typecheck

loadFile :: MonadResource m => String -> Source m (Either String Register)
loadFile path = sourceFile path $= bytesToChar =$= compile

printout :: (MonadIO m, Show a) => Sink a m ()
printout = awaitForever (liftIO . print)
