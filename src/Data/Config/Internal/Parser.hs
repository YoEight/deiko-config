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
module Data.Config.Internal.Parser (lalrParser) where

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
            ITspace   -> skipSpaceOrNewline >> deeper
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
    exp <- inner
    return $ PropBind pId exp
  where
    equalExpr = do
        parseEqual
        skipSpaceOrNewline
        parseLExpr

    inner = do
        t <- lookahead
        if isObrace t
            then parseObject
            else equalExpr

parseId :: Parser T.Text
parseId = shift >>= go
  where
    go l@(L _ (ITvarid id)) = return id
    go l                    = expected l "an id"

parseLExpr :: Parser (LExpr T.Text)
parseLExpr = do
    t     <- lookahead
    e     <- go t
    L _ t <- lookahead
    case t of
        ITspace -> deeper e
        _       -> return e

  where
    deeper e@(L sp _) = do
        shift
        t <- lookahead
        if not (isExpr t)
            then return e
            else do
            e1 <- parseLExpr
            return $ L sp (Merge e e1)
    go t
        | isObrack t = parseList
        | isLit t    = parseLit
        | isObrace t = parseObject
        | isDollar t = parseSubst
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
    L ep _ <- parseCbrack
    return $ L (mkPointSpan sp ep) (List undefined xs)
  where
    parseExprList = do
        skipSpaceOrNewline
        L se t <- lookahead
        case t of
            ITcbrack -> return []
            _        -> (:) <$> parseLExpr <*> go

    go = do
        skipSpaceOrNewline
        L _ t <- lookahead
        case t of
            ITcbrack -> return []
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

parseObject :: Parser (LExpr T.Text)
parseObject = do
    L sp _ <- parseObrace
    ds     <- parseBinds
    L ep _ <- parseCbrace
    return $ L (mkPointSpan sp ep) (Object ds)
  where
    parseBinds = do
        skipSpaceOrNewline
        L _ t <- lookahead
        case t of
            ITcbrace -> return []
            _ -> do
                b  <- parseBind
                bs <- deeper
                return (b:bs)

    deeper = do
        L _ t <- lookahead
        case t of
            ITcbrace  -> return []
            ITspace   -> skipSpaceOrNewline >> deeper
            ITnewline -> skipSpaceOrNewline >> parseBinds
            ITcomma   -> do
                shift
                skipSpaceOrNewline
                b  <- parseBind
                bs <- deeper
                return (b:bs)

parseSubst :: Parser (LExpr T.Text)
parseSubst = do
    L sp _  <- parseDollar
    parseObrace
    s       <- parseId
    L ep _  <- parseCbrace
    return $ L (mkPointSpan sp ep) (Subst s)

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

parseObrace :: Parser Token
parseObrace = shift >>= go
  where
    go t
        | isObrace t = return t
        | otherwise  = expected t "a '{'"

parseCbrace :: Parser Token
parseCbrace = shift >>= go
  where
    go t
        | isCbrace t = return t
        | otherwise  = expected t "a '}'"

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

parseDollar :: Parser Token
parseDollar = shift >>= go
  where
    go t
        | isDollar t = return t
        | otherwise  = expected t "a '$'"

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

isSpace :: Token -> Bool
isSpace (L _ ITspace) = True
isSpace _             = False

isObrack :: Token -> Bool
isObrack (L _ ITobrack) = True
isObrack _              = False

isObrace :: Token -> Bool
isObrace (L _ ITobrace) = True
isObrace _              = False

isCbrace :: Token -> Bool
isCbrace (L _ ITcbrace) = True
isCbrace _              = False

isDollar :: Token -> Bool
isDollar (L _ ITdollar) = True
isDollar _              = False

isExpr :: Token -> Bool
isExpr t =
    isObrack t ||
    isLit t    ||
    isObrace t ||
    isDollar t

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
