{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Parser
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Parser (parse) where

--------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$), (<$>))
import Control.Exception hiding (try)
import Data.Functor (void)
import Data.Monoid ((<>))
import Data.Typeable

--------------------------------------------------------------------------------
import           Data.Text (Text, pack)
import qualified Data.Text         as T
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text ()
import qualified Text.Parsec.Token as P

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Pos

--------------------------------------------------------------------------------
newtype PlainError = PlainError String deriving Typeable

--------------------------------------------------------------------------------
instance Show PlainError where
    show (PlainError s) = s

--------------------------------------------------------------------------------
instance Exception PlainError

--------------------------------------------------------------------------------
parse :: Monad m => FilePath -> Text -> m (Either SomeException [Prop AST Pos])
parse path input
    = do ps <- runParserT parsePROPS () path input
         return $ either (Left . SomeException . PlainError . show) Right ps

--------------------------------------------------------------------------------
-- | Language definition
--------------------------------------------------------------------------------
langDef :: Monad m => P.GenLanguageDef Text u m
langDef
    = P.LanguageDef
      { P.commentStart    = ""
      , P.commentEnd      = ""
      , P.commentLine     = ""
      , P.nestedComments  = False
      , P.identStart      = letter <|> char '_'
      , P.identLetter     = alphaNum <|> oneOf "-_"
      , P.opStart         = P.opLetter langDef
      , P.opLetter        = oneOf ":.{}[]="
      , P.reservedNames   = []
      , P.reservedOpNames = []
      , P.caseSensitive   = True
      }

--------------------------------------------------------------------------------
-- | Token parser
--------------------------------------------------------------------------------
tokenParser :: Monad m => P.GenTokenParser Text u m
tokenParser = P.makeTokenParser langDef

--------------------------------------------------------------------------------
-- | Property based parsing
--------------------------------------------------------------------------------
parsePROPS :: Monad m => ParsecT Text u m [Prop AST Pos]
parsePROPS = do
    skipMany (parseCOMMENT >> whitespace)
    whitespace
    properties
  where
    properties = commonPROPS eof

--------------------------------------------------------------------------------
parsePROP :: Monad m => ParsecT Text u m (Prop AST Pos)
parsePROP = do
    AST (ID i) _ <- parseIDENT
    v <- parseOBJECT <|> do { _ <- equal; optional whitespace; parseVALUE }
    return $ Prop i v
  where
    equal = char '=' <|> char ':'

--------------------------------------------------------------------------------
parseOBJECT :: Monad m => ParsecT Text u m (AST Pos)
parseOBJECT = do
    ps <- getPosition
    pp <- between (char '{') (char '}') $ do
        skipMany (parseCOMMENT >> whitespace)
        whitespace
        p <- objProperties
        skipMany (parseCOMMENT >> whitespace)
        whitespace
        return p
    pe <- getPosition
    return $ AST (OBJECT pp) (mkPos ps pe)
  where
    objProperties = option [] $ commonPROPS (void $ lookAhead $ char '}')

--------------------------------------------------------------------------------
commonPROPS :: Monad m => ParsecT Text u m () -> ParsecT Text u m [Prop AST Pos]
commonPROPS end = do
    p <- parsePROP
    skipMany (parseCOMMENT >> whitespace)
    whitespace
    ps <- ([] <$ end) <|>
          do { optional $ do
                    _ <- comma
                    whitespace
                    optional parseCOMMENT
                    whitespace
             ; commonPROPS end
             }
    return (p:ps)

--------------------------------------------------------------------------------
parseIDENT :: Monad m => ParsecT Text u m (AST Pos)
parseIDENT = do
    p <- getPosition
    i <- ident

    let ti  = pack i
        ls  = fromIntegral $ sourceLine p
        cs  = fromIntegral $ sourceColumn p
        ce1 = T.length ti
        a1  = AST (ID ti) (Line ls cs ce1)

    t <- optionMaybe (dot >> parseIDENT)

    let onTail (AST (ID is) (Line _ _ ce2))
            = AST (ID (ti <> "." <> is)) (Line ls cs ce2)
        onTail _
            = error "impossible situation onTail"

    return $ maybe a1 onTail t
  where
    dot   = char '.'
    ident = P.identifier tokenParser

--------------------------------------------------------------------------------
parseVALUE :: Monad m => ParsecT Text u m (AST Pos)
parseVALUE =
    chainr1 inner $ do
        _ <- try $ ((many1 $ char ' ') >> notFollowedBy (oneOf "\n}],;"))
        return mkMerge
  where
    inner = parseLIST <|> parseOBJECT <|> parseSUBST <|> parseSTRING

    mkMerge x@(AST _ px) y@(AST _ py)
        = let ls  = startLine px
              le  = endLine py
              cs  = startCol px
              ce  = endCol py
              pos = if ls == le
                    then Line ls cs ce
                    else Multi ls le cs ce  in
          AST (MERGE x y) pos

--------------------------------------------------------------------------------
parseLIST :: Monad m => ParsecT Text u m (AST Pos)
parseLIST = do
    ps <- getPosition
    vs <- between (char '[') (char ']') $ do
        skipMany (parseCOMMENT >> whitespace)
        whitespace
        commaSep $ do
            optional whitespace
            v <- parseVALUE
            skipMany (parseCOMMENT >> whitespace)
            optional whitespace
            return v

    pe <- getPosition
    return $ AST (LIST vs) (mkPos ps pe)
  where
    commaSep = P.commaSep tokenParser

--------------------------------------------------------------------------------
parseCOMMENT :: Monad m => ParsecT Text u m ()
parseCOMMENT = do
    _ <- string "#"
    skipMany (satisfy (/= '\n'))

--------------------------------------------------------------------------------
parseSUBST :: Monad m => ParsecT Text u m (AST Pos)
parseSUBST = do
    ps           <- getPosition
    _            <- string "${"
    AST (ID i) _ <- parseIDENT
    _            <- char '}'
    pe           <- getPosition
    return $ AST (SUBST i) (mkPos ps pe)

--------------------------------------------------------------------------------
-- | String literal parsing
--------------------------------------------------------------------------------
parseSTRING :: Monad m => ParsecT Text u m (AST Pos)
parseSTRING = parseMultiSTRING <|> parseSimpleSTRING <|> parseNakedSTRING

--------------------------------------------------------------------------------
-- | Parse anything between """, newlines included
parseMultiSTRING :: Monad m => ParsecT Text u m (AST Pos)
parseMultiSTRING = do
    ps <- getPosition
    s  <- between (try $ tripleQuote) tripleQuote $ do
        let loop = do
                xs <- many $ noneOf "\""
                (xs <$ (lookAhead tripleQuote)) <|>
                    (\c cs -> xs ++ c:cs) <$> char '"' <*> loop
        loop
    pe <- getPosition
    return $ AST (STRING $ pack s) (mkPos ps pe)
  where
    tripleQuote = string "\"\"\""

--------------------------------------------------------------------------------
-- | Parse anything between " as long as it doesn't include newline
parseSimpleSTRING :: Monad m => ParsecT Text u m (AST Pos)
parseSimpleSTRING = do
    ps <- getPosition
    s  <- between simpleQuote simpleQuote (many $ noneOf "\"\n")
    pe <- getPosition
    return $ AST (STRING $ pack s) (mkPos ps pe)
  where
    simpleQuote = char '"'

--------------------------------------------------------------------------------
parseNakedSTRING :: Monad m => ParsecT Text u m (AST Pos)
parseNakedSTRING = do
    ps <- getPosition
    s  <- many1 $ noneOf " #,{}[]\n"
    pe <- getPosition
    return $ AST (STRING $ pack s) (mkPos ps pe)

--------------------------------------------------------------------------------
-- | Utilities
--------------------------------------------------------------------------------
whitespace :: Monad m => ParsecT Text u m ()
whitespace = P.whiteSpace tokenParser

--------------------------------------------------------------------------------
comma :: Monad m => ParsecT Text u m ()
comma = void $ char ','

--------------------------------------------------------------------------------
mkPos :: SourcePos -> SourcePos -> Pos
mkPos ps pe =
    let ls = fromIntegral $ sourceLine ps
        cs = fromIntegral $ sourceColumn ps
        le = fromIntegral $ sourceLine pe
        ce = fromIntegral $ sourceColumn pe in
    if ls == le
    then Line ls cs ce
    else Multi ls le cs ce
