{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Config.Internal.Parser
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Config.Internal.Parser where

import Control.Monad (ap)
import Control.Applicative hiding ((<|>), optional)

import qualified Data.Text as T
import           Text.Parsec.Prim (parserFail)

import Data.Config.Internal.Lexer
import Data.Config.Internal.SrcLoc
import Data.Config.Internal.Syn
import Data.Config.Internal.Token

import Debug.Trace

newtype Parser a =
    Parser { unParser :: Maybe Token -> Lexer (Maybe Token, Either String a) }

instance Functor Parser where
    fmap f (Parser k) =
        Parser $ \s -> do
            (s', e_a) <- k s
            return (s', fmap f e_a)

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return a = Parser (\s -> return (s, Right a))

    Parser ka >>= f =
        Parser $ \s -> do
            (s', e_a) <- ka s
            case e_a of
                Left e  -> return (s', Left e)
                Right a -> unParser (f a) s'

lookahead :: Parser Token
lookahead =
    Parser $ \s ->
    case s of
        Just t -> return (s, Right t)
        _      -> do
            t <- lexToken
            return (Just t, Right t)

shift :: Parser Token
shift =
    Parser $ \s ->
    case s of
        Just t -> return (Nothing, Right t)
        _      -> do
            t <- lexToken
            return (Nothing, Right t)

lalrParser :: Lexer (Module T.Text)
lalrParser = do
    (_, e_a) <- unParser parseModule Nothing
    either parserFail return e_a

parseModule :: Parser (Module T.Text)
parseModule = do
   xs <- parseDecls
   return $ Module [] xs

parseDecls :: Parser [Decl T.Text]
parseDecls = lookahead >>= go
  where
    go (L _ ITeof) = [] <$ shift
    go _           = do
        d  <- parseDecl
        ds <- deeper
        return (d:ds)

    deeper = do
        L _ t <- lookahead
        case t of
            ITeof     -> shift >> return []
            ITspace   -> skipSpaceOrNewline >> eof
            ITnewline -> skipSpaceOrNewline >> parseDecls
            ITcomma   -> do
                shift
                skipSpaceOrNewline
                d  <- parseDecl
                ds <- deeper
                return (d:ds)
    eof = [] <$ parseEOF
parseDecl :: Parser (Decl T.Text)
parseDecl = parseValD

parseValD :: Parser (Decl T.Text)
parseValD = fmap ValD parseBind

parseBind :: Parser (Bind T.Text)
parseBind = do
    pId <- parseId
    skipSpaceOrNewline
    parseEqual
    skipSpaceOrNewline
    exp <- parseLExpr
    return $ PropBind pId exp

parseId :: Parser T.Text
parseId = shift >>= go
  where
    go l@(L _ (ITvarid id)) = return id
    go l                    = expected l "an id"

parseLExpr :: Parser (LExpr T.Text)
parseLExpr = lookahead >>= go
  where
    go t
        | isObrack t = parseList
        | isLit t    = parseLit
        | otherwise  = failure ("Unexpected " ++ show t)

parseLit :: Parser (LExpr T.Text)
parseLit = fmap go shift
  where go (L sp t)
            | ITvarid s  <- t = L sp (Lit $ LString s)
            | ITstring s <- t = L sp (Lit $ LString s)

parseList :: Parser (LExpr T.Text)
parseList = do
    L sp _ <- parseObrack
    xs     <- parseExprList
    return $ L sp (List undefined xs)
  where
    parseExprList = do
        skipSpaceOrNewline
        L _ t <- lookahead
        case t of
            ITcbrack -> shift >> return []
            _        -> (:) <$> parseLExpr <*> go

    go = do
        skipSpaceOrNewline
        L _ t <- lookahead
        case t of
            ITcbrack -> shift >> return []
            ITcomma  -> do
                shift
                skipSpaceOrNewline
                e  <- parseLExpr
                es <- go
                return (e:es)
            _ ->
                let msg = "Unexpected " ++ show t ++
                          " when constructing a list" in
                failure msg

skipSpaceOrNewline :: Parser ()
skipSpaceOrNewline = lookahead >>= go
  where
    go (L _ ITspace)   = shift >> skipSpaceOrNewline
    go (L _ ITnewline) = shift >> skipSpaceOrNewline
    go _               = return ()

parseObrack :: Parser Token
parseObrack = shift >>= go
  where
    go t@(L _ ITobrack) = return t
    go l                = expected l "a ']'"

parseCbrack :: Parser Token
parseCbrack = shift >>= go
  where
    go t@(L _ ITcbrack) = return t
    go l                = expected l "a '['"

parseSpace :: Parser ()
parseSpace = lookahead >>= go
  where
    go (L _ ITspace) = shift >> return ()
    go l             = expected l "a space character"

parseNewline :: Parser ()
parseNewline = lookahead >>= go
  where
    go (L _ ITnewline) = shift >> return ()
    go l               = expected l "newline"

parseEqual :: Parser ()
parseEqual = shift >>= go
  where
    go (L _ ITequal) = return ()
    go l             = expected l "a '='"

parseComma :: Parser ()
parseComma = lookahead >>= go
  where
    go (L _ ITcomma) = shift >> return ()
    go l             = expected l "a ','"

parseEOF :: Parser ()
parseEOF = lookahead >>= go
  where
    go (L _ ITeof) = shift >> return ()
    go _           = failure "Expected end of file"

isVarId :: Token -> Bool
isVarId (L _ (ITvarid _)) = True
isVarId _                 = False

isDot :: Token -> Bool
isDot (L _ ITdot) = True
isDot _           = False

isLit :: Token -> Bool
isLit (L _ (ITstring _)) = True
isLit t                  = isVarId t

isEqual :: Token -> Bool
isEqual (L _ ITequal) = True
isEqual _             = False

isSpace :: Token -> Bool
isSpace (L _ ITspace) = True
isSpace _             = False

isObrack :: Token -> Bool
isObrack (L _ ITobrack) = True
isObrack _              = False

isCbrack :: Token -> Bool
isCbrack (L _ ITcbrack) = True
isCbrack _              = False

isComma :: Token -> Bool
isComma (L _ ITcomma) = True
isComma _             = False

isNewline :: Token -> Bool
isNewline (L _ ITnewline) = True
isNewline _               = False

isEOF :: Token -> Bool
isEOF (L _ ITeof) = True
isEOF _           = False

mergeSSOneLine :: SrcSpan -> SrcSpan -> SrcSpan
mergeSSOneLine (SrcSpanOneLine n l s _) (SrcSpanOneLine _ _ _ e) =
    SrcSpanOneLine n l s e

expected :: Token -> String -> Parser a
expected (L l t) e = failure msg
  where
    msg = "Found " ++ show t ++ " at " ++ show l ++
          " while expecting " ++ e

failure :: String -> Parser a
failure msg = Parser (\s -> return (s, Left msg))
