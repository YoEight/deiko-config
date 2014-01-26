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
parseModule = fmap (Module []) parseDecls

parseDecls :: Parser [Decl T.Text]
parseDecls = lookahead >>= go
  where
    go t
        | isEOF t   = [] <$ shift
        | otherwise = do
            d  <- parseDecl
            ds <- deeper
            return (d:ds)

    deeper = do
        t <- lookahead
        case () of
            _ | isEOF t     -> [] <$ shift
              | isSpace t   -> skipSpaceOrNewline >> deeper
              | isNewline t -> skipSpaceOrNewline >> parseDecls
              | isComma t   -> onComma

    onComma = do
        shift
        skipSpaceOrNewline
        d  <- parseDecl
        ds <- deeper
        return (d:ds)

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
    e  <- lookahead >>= go
    t2 <- lookahead
    if isSpace t2
        then deeper e
        else return e
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
        t <- lookahead
        if isCbrack t
            then return []
            else (:) <$> parseLExpr <*> go

    go = do
        skipSpaceOrNewline
        t <- lookahead
        case () of
            _ | isCbrack t -> return []
              | isComma t  -> onComma
              | otherwise  ->
                let msg = "Unexpected " ++ show t ++
                          " when constructing a list" in
                failure msg

    onComma = do
        shift
        skipSpaceOrNewline
        e  <- parseLExpr
        es <- go
        return (e:es)

parseObject :: Parser (LExpr T.Text)
parseObject = do
    L sp _ <- parseObrace
    ds     <- parseBinds
    L ep _ <- parseCbrace
    return $ L (mkPointSpan sp ep) (Object ds)
  where
    parseBinds = do
        skipSpaceOrNewline
        t <- lookahead
        if isCbrace t
            then return []
            else onComma

    onComma = do
        b  <- parseBind
        bs <- deeper
        return (b:bs)

    deeper = do
        t <- lookahead
        case () of
            _ | isCbrace t  -> return []
              | isSpace t   -> skipSpaceOrNewline >> deeper
              | isNewline t -> skipSpaceOrNewline >> parseBinds
              | isComma t   -> do
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
    go t
        | isSpace t || isNewline t = shift >> skipSpaceOrNewline
        | otherwise                = return ()

parseObrack :: Parser Token
parseObrack = shift >>= go
  where
    go t
        | isObrack t = return t
        | otherwise  = expected t "a '['"

parseCbrack :: Parser Token
parseCbrack = shift >>= go
  where
    go t
        | isCbrack t = return t
        | otherwise  = expected t "a ']'"

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

parseEqual :: Parser ()
parseEqual = shift >>= go
  where
    go t
        | isEqual t = return ()
        | otherwise = expected t "a '='"

parseComma :: Parser ()
parseComma = shift >>= go
  where
    go t
        | isComma t = return ()
        | otherwise = expected t "a ','"

parseDollar :: Parser Token
parseDollar = shift >>= go
  where
    go t
        | isDollar t = return t
        | otherwise  = expected t "a '$'"

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

isNewline :: Token -> Bool
isNewline (L _ ITnewline) = True
isNewline _               = False

isObrack :: Token -> Bool
isObrack (L _ ITobrack) = True
isObrack _              = False

isCbrack :: Token -> Bool
isCbrack (L _ ITcbrack) = True
isCbrack _              = False

isObrace :: Token -> Bool
isObrace (L _ ITobrace) = True
isObrace _              = False

isCbrace :: Token -> Bool
isCbrace (L _ ITcbrace) = True
isCbrace _              = False

isDollar :: Token -> Bool
isDollar (L _ ITdollar) = True
isDollar _              = False

isEqual :: Token -> Bool
isEqual (L _ ITequal) = True
isEqual _             = False

isComma :: Token -> Bool
isComma (L _ ITcomma) = True
isComma _             = False

isEOF :: Token -> Bool
isEOF (L _ ITeof) = True
isEOF _           = False

isExpr :: Token -> Bool
isExpr t =
    isObrack t ||
    isLit t    ||
    isObrace t ||
    isDollar t

expected :: Token -> String -> Parser a
expected (L l t) e = failure msg
  where
    msg = "Found " ++ show t ++ " at " ++ show l ++
          " while expecting " ++ e

failure :: String -> Parser a
failure msg = Parser (\s -> return (s, Left msg))
