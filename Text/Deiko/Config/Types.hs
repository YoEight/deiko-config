{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes                #-}
module Text.Deiko.Config.Types where

import Control.Applicative (WrappedMonad (..), (<*), (*>), (<*>), (<$>))
import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader (MonadReader)
import Data.Char (isDigit)
import Data.Functor.Identity (Identity)
import Data.Hashable
import Data.Monoid (Monoid, (<>))
import Data.String (IsString (..))
import Data.Traversable (traverse)
import Text.Deiko.Config.Internal hiding (string)
import Text.Deiko.Config.Util
import Text.Parsec.Pos (newPos)
import Text.Parsec.Prim (Parsec, Stream, parse, getPosition, setPosition, unexpected, (<|>))
import Text.ParserCombinators.Parsec.Char (digit, anyChar, char, string)
import Text.ParserCombinators.Parsec.Combinator (many1, eof)

class ( StringLike s
      , MonadReader (TypeTable s) m
      , MonadError (ConfigError s) m) => ConfigCtx s m

type Conversion s v =
  forall m. (MonadReader (TypeTable s) m, MonadError (ConfigError s) m)
  => s
  -> Type s
  -> Annoted s
  -> m v

stringValue :: StringLike s => Conversion s s
stringValue key typ value =
  sameType typ stringType >>= \same ->
    case () of
      _ | same      -> cata (\(ASTRING _ s) -> return s) value
        | otherwise ->
          showType typ >>= \typStr ->
          throwError $ wrongType key "String" typStr

intParsec :: (StringLike s, Stream s Identity Char) => Parsec s () Int
intParsec = fmap read (many1 digit <* eof)

intValue :: (StringLike s, Stream s Identity Char) => Conversion s Int
intValue = parsecValue "Int" intParsec

boolValue :: (StringLike s, Stream s Identity Char) => Conversion s Bool
boolValue = parsecValue "Bool" boolParsec

boolParsec :: (StringLike s, Stream s Identity Char) => Parsec s () Bool
boolParsec = (fmap (const True) (string "true")     <|>
              fmap (const True) (string "True")     <|>
              fmap (const True) (string "yes")      <|>
              fmap (const True) (string "Yes")      <|>
              fmap (const False) (string "false")   <|>
              fmap (const False) (string "False")   <|>
              fmap (const False) (string "no")      <|>
              fmap (const False) (string "No")      <|>
              onOff) <* eof
  where
    onOff = do
      char 'o' <|> char 'O'
      c <- anyChar
      case c of
        'n' -> return True
        'f' -> char 'f' >> return False
        _   -> unexpected (show c ++ " when parsing on|off or On|Off")

parsecValue :: (StringLike s, Stream s Identity Char) => s -> Parsec s () a -> Conversion s a
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

listValue :: StringLike s => Conversion s v -> Conversion s [v]
listValue k key typ value = maybe onError ((flip go) value) (listOf typ)
  where
    ctx e typ = "In context of parsing a List[" <> typ <> "]: " <> e
    onError = showType typ >>= (throwError . wrongType key "List")
    go typ (Mu (ALIST _ xs)) =
      let action = unwrapMonad $ traverse (WrapMonad . k key typ) xs
          handler (ConfigError e) =
            showType typ >>= (throwError . ConfigError . ctx e) in
      catchError action handler

propertyNotFound :: (IsString s, Monoid s) => s -> ConfigError s
propertyNotFound prop = ConfigError ("Property [" <> prop <> "] not found")

wrongType :: (IsString s, Monoid s) => s -> s -> s -> ConfigError s
wrongType key expected found = ConfigError msg
  where
    msg = "Type error on [" <> key <> "] when getting value. expected: "
          <> expected <> ", found: " <> found

-- parseError :: String -> String -> String -> ConfigError
-- parseError key val typ = ConfigError msg
--   where
--     msg = "Can't parse a " ++ typ ++ " for [" ++ key ++ "] with value ["
--           ++ val ++ "]"

-- parseTypeError :: String -> String -> Type -> ConfigError
-- parseTypeError key expected found = ConfigError msg
--   where
--     fouStr = showType found
--     msg = "Can't parse a " ++ expected ++ " with a value of type " ++ fouStr
--           ++ " for [" ++ key ++ "]"
