{-# LANGUAGE RankNTypes #-}
module Text.Deiko.Config where

import Control.Monad.Trans (MonadIO (..))
import Data.Conduit (Conduit, Producer, Sink, ($=), ($$), (=$=), yield
                    ,awaitForever, runResourceT)
import Data.Conduit.Binary (sourceFile)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Foldable (traverse_)
import Text.Deiko.Config.Lexer (lexer)
import Text.Deiko.Config.Parser (Mu, AST, parser, printer) 

bytesToChar :: Monad m => Conduit ByteString m Char
bytesToChar = awaitForever (traverse_ yield . unpack)

sourceString :: Monad m => String -> Producer m Char
sourceString = traverse_ yield

loadFile :: String -> IO (Either String (Mu AST))
loadFile path = 
  let action = sourceFile path $= bytesToChar =$= lexer $$ parser in
  runResourceT action  >>= \r ->
     fmap (const r) (printer r)

printout :: (MonadIO m, Show a) => Sink a m ()
printout = awaitForever (liftIO . print)
