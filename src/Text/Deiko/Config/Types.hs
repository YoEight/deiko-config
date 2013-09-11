{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
module Text.Deiko.Config.Types where

import Control.Applicative (WrappedMonad (..), (<*), (*>), (<*>), (<$>))
import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader (MonadReader)
import Data.Char (isDigit)
import Data.Monoid (Monoid, (<>))
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Traversable (traverse)
import Text.Deiko.Config.Internal hiding (string)
import Text.Deiko.Config.Util
import Text.Parsec.Pos (newPos)
import Text.Parsec.Prim (Parsec, Stream, parse, getPosition, setPosition, unexpected, (<|>))
import Text.Parsec.Text
import Text.ParserCombinators.Parsec.Char (digit, anyChar, char, string)
import Text.ParserCombinators.Parsec.Combinator (many1, eof)

class (MonadReader TypeTable m, MonadError ConfigError m) => ConfigCtx m

type Conversion v =
  forall m. (MonadReader TypeTable m, MonadError ConfigError m)
  => T.Text
  -> Type
  -> Annoted
  -> m v

stringValue :: Conversion T.Text
stringValue key typ value =
  sameType typ stringType >>= \same ->
    case () of
      _ | same      -> cata go value
        | otherwise ->
          showType typ >>= \typStr ->
          throwError $ wrongType key "String" typStr
  where
    go (ASTRING _ s) = return s
    go (ASUBST _ s)  = throwError (unresolved key s "String")

intParsec :: Parsec T.Text () Int
intParsec = fmap read (many1 digit <* eof)

intValue :: Conversion Int
intValue = parsecValue "Int" intParsec

boolValue :: Conversion Bool
boolValue = parsecValue "Bool" boolParsec

boolParsec :: Parsec T.Text () Bool
boolParsec = (fmap (const True) (string "true")   <|>
              fmap (const True) (string "True")   <|>
              fmap (const True) (string "yes")    <|>
              fmap (const True) (string "Yes")    <|>
              fmap (const False) (string "false") <|>
              fmap (const False) (string "False") <|>
              fmap (const False) (string "no")    <|>
              fmap (const False) (string "No")    <|>
              onOff) <* eof
  where
    onOff = do
      char 'o' <|> char 'O'
      c <- anyChar
      case c of
        'n' -> return True
        'f' -> char 'f' >> return False
        _   -> unexpected (show c ++ " when parsing on|off or On|Off")

parsecValue :: T.Text -> Parsec T.Text () a -> Conversion a
parsecValue ptyp parser key typ value =
  catchError (stringValue key typ value) handler >>= \s ->
    let (l, c) = pos value
        upd _  = newPos (show key) l c
        init   = (setPosition . upd) =<< getPosition
        err e  = throwError $ ConfigError (ctx <> fromString e)
        result = parse (init >> parser) "" s in
    either (err . show) return result
  where
    ctx = "In context of parsing " <> ptyp <> ": "
    handler (ConfigError e) = throwError $ ConfigError (ctx <> e)

listValue :: Conversion v -> Conversion [v]
listValue k key typ value = maybe onError ((flip go) value) (listOf typ)
  where
    ctx e typ = "In context of parsing a List[" <> typ <> "]: " <> e
    onError = showType typ >>= (throwError . wrongType key "List")
    go typ (Mu (ALIST _ xs)) =
      let action = unwrapMonad $ traverse (WrapMonad . k key typ) xs
          handler (ConfigError e) =
            showType typ >>= (throwError . ConfigError . ctx e) in
      catchError action handler
    go typ (Mu (ASUBST _ s)) = throwError (unresolved key s "List")

propertyNotFound :: T.Text -> ConfigError
propertyNotFound prop = ConfigError ("Property [" <> prop <> "] not found")

wrongType :: T.Text -> T.Text -> T.Text -> ConfigError
wrongType key expected found = ConfigError msg
  where
    msg = "Type error on [" <> key <> "] when getting value. expected: "
          <> expected <> ", found: " <> found

unresolved :: T.Text -> T.Text -> T.Text -> ConfigError
unresolved key subst typ = ConfigError msg
  where
    msg = "Can't make a " <> typ <> " for [" <> key <> "] because [" <>
          subst <> "] is unresolved"
