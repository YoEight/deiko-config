-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Config.Internal.Lexer
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Config.Internal.Lexer
       ( PState (..)
       , Lexer
       , lexToken
       , initState
       , parseUntilEOF
       ) where

import Control.Applicative ((<*), (<$))
import Control.Monad (when, join)
import Debug.Trace

import qualified Data.Text as T
import           Text.Parsec.Char -- a lot
import           Text.Parsec.Combinator
import           Text.Parsec.Pos
import           Text.Parsec.Prim -- a lot
import           Text.Parsec.Text

import Data.Config.Internal.SrcLoc
import Data.Config.Internal.Token

data PState = PState
              { spBraceCount :: !Int
              , spBrackCount :: !Int
              }

type Lexer = GenParser PState

initState = PState 0 0

symChars = ".=:,${}[]"

spaceOrTab :: Char -> Bool
spaceOrTab c
    | ' '  <- c = True
    | '\t' <- c = True
    | otherwise = False

braceCount :: (Int -> Int) -> Lexer ()
braceCount k = modifyState go
  where
    go s@PState{spBraceCount=i} = s{spBraceCount=k i}

incrBraceCount, decrBraceCount :: Lexer ()
incrBraceCount = braceCount succ
decrBraceCount = braceCount pred

brackCount :: (Int -> Int) -> Lexer ()
brackCount k = modifyState go
  where
    go s@PState{spBrackCount=i} = s{spBrackCount=k i}

incrBrackCount, decrBrackCount :: Lexer ()
incrBrackCount = brackCount succ
decrBrackCount = brackCount pred

lexToken :: Lexer Token
lexToken = optional lexComment >> action
  where
    action = lexKeyword    <|>
             lexSymbol     <|>
             lexSpace      <|>
             lexNewline    <|>
             lexIdentifier <|>
             lexString     <|>
             lexEof

lexComment :: Lexer ()
lexComment = do
    char '#'
    manyTill anyChar newline
    skipMany newline

lexKeyword :: Lexer Token
lexKeyword = try include
  where
    include = do
        poss <- getPosition
        string "include"
        pose <- getPosition
        let name = sourceName poss
            line = sourceLine poss
            scol = sourceColumn poss
            ecol = sourceColumn pose
            span = SrcSpanOneLine name line scol ecol
            tok  = L span ITinclude
        return tok

lexSymbol :: Lexer Token
lexSymbol = go =<< oneOf symChars
  where
    go '.' = mkPointToken ITdot
    go '=' = mkPointToken ITequal
    go ':' = mkPointToken ITequal
    go ',' = mkPointToken ITcomma
    go '$' = mkPointToken ITdollar
    go '{' = incrBraceCount >> mkPointToken ITobrace
    go '}' = decrBraceCount >> mkPointToken ITcbrace
    go '[' = incrBrackCount >> mkPointToken ITobrack
    go ']' = decrBrackCount >> mkPointToken ITcbrack

lexSpace :: Lexer Token
lexSpace = do
    space
    mkPointToken ITspace <* skipMany space
  where
    space = satisfy spaceOrTab

lexNewline :: Lexer Token
lexNewline = do
    newline
    mkPointToken ITnewline <* skipMany newline

lexIdentifier :: Lexer Token
lexIdentifier = do
    poss <- getPosition
    xs   <- chainl1 (many1 letter) aDot
    pose <- getPosition
    let name = sourceName poss
        line = sourceLine poss
        scol = sourceColumn poss
        ecol = sourceColumn pose
        span = SrcSpanOneLine name line scol ecol
        itok = ITvarid $ T.pack xs
        tok  = L span itok
    return tok
  where
    aDot = fmap (\c a b -> a ++ (c:b)) (char '.')

lexString :: Lexer Token
lexString = try multi <|> simple <|> anythingElse
  where
    simple = do
        poss <- getPosition
        char '"'
        xs   <- manyTill anything (char '"') <?> "Unterminated String literal"
        pose <- getPosition
        let name = sourceName poss
            line = sourceLine poss
            scol = sourceColumn poss
            ecol = sourceColumn pose
            span = SrcSpanOneLine name line scol ecol
            itok = ITstring $ T.pack xs
            tok  = L span itok
        return tok

    multi = do
        poss <- getPosition
        quote3
        xs   <- manyTill anyChar quote3 <?> "Unterminated String literal"
        pose <- getPosition
        let name = sourceName poss
            slin = sourceLine poss
            scol = sourceColumn poss
            elin = sourceLine pose
            ecol = sourceColumn pose
            span = SrcSpanMultiLine name slin elin scol ecol
            itok = ITstring $ T.pack xs
            tok  = L span itok
        return tok

    anythingElse = do
        poss <- getPosition
        let limiters = " \n\t" ++ symChars
        xs   <- manyTill (noneOf limiters) (lookAhead $ oneOf limiters)
        pose <- getPosition
        let name = sourceName poss
            line = sourceLine poss
            scol = sourceColumn poss
            ecol = sourceColumn pose
            span = SrcSpanOneLine name line scol ecol
            itok = ITstring $ T.pack xs
            tok  = L span itok
        return tok

    anything = noneOf "\n"

    quote3 = string "\"\"\""

lexEof :: Lexer Token
lexEof = do
    eof
    PState braceC brackC <- getState
    when (braceC /= 0) (parserFail "Unmatched ')'")
    when (brackC /= 0) (parserFail "Unmatched ']'")
    mkPointToken ITeof

mkPointToken :: IToken -> Lexer Token
mkPointToken t = do
    pos <- getPosition
    let line = sourceLine pos
        col  = sourceColumn pos
        name = sourceName pos
        span = SrcSpanPoint name line col
        tok  = L span t
    return tok

isEOF :: Token -> Bool
isEOF (L _ ITeof) = True
isEOF _           = False

parseUntilEOF :: Lexer [Token]
parseUntilEOF = do
    t <- lexToken
    if isEOF t
        then return [t]
        else fmap (t:) parseUntilEOF
