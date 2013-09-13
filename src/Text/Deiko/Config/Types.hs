{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
module Text.Deiko.Config.Types where

import Prelude hiding (catch)
import Control.Applicative (WrappedMonad (..), (<*), (*>), (<*>), (<$>))
import Control.Monad.Catch (MonadCatch, throwM, catch)
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

type Conversion v =
  forall m. (MonadCatch m, MonadReader TypeTable m)
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
          throwM $ wrongType key "String" typStr
  where
    go (ASTRING _ s) = return s
    go (ASUBST _ s)  = throwM (unresolved key s "String")

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
  catch (stringValue key typ value) handler >>= \s ->
    let (l, c) = pos value
        upd _  = newPos (show key) l c
        init   = (setPosition . upd) =<< getPosition
        err e  = throwM $ ConfigError (ctx <> fromString e)
        result = parse (init >> parser) "" s in
    either (err . show) return result
  where
    ctx = "In context of parsing " <> ptyp <> ": "
    handler (ConfigError e) = throwM $ ConfigError (ctx <> e)

listValue :: Conversion v -> Conversion [v]
listValue k key typ value = maybe onError ((flip go) value) (listOf typ)
  where
    ctx e typ = "In context of parsing a List[" <> typ <> "]: " <> e
    onError = showType typ >>= (throwM . wrongType key "List")
    go typ (Mu (ALIST _ xs)) =
      let action = unwrapMonad $ traverse (WrapMonad . k key typ) xs
          handler (ConfigError e) =
            showType typ >>= (throwM . ConfigError . ctx e) in
      catch action handler
    go typ (Mu (ASUBST _ s)) = throwM (unresolved key s "List")

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
