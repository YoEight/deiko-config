{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- Config is a small and typesafe configuration library. It provides
-- good error messages and comes with a bottom-up typechecker in order to catch
-- more configuration errors.
--
-- Here some use-cases:
--
-- >>> foo = ["bar", { baz : 42 }]
-- foo:1:8-13: Expecting String but having Object
--
-- Reason: List has only one inner type
--
-- >>> foo = ["bar"] [{ baz : 42 }]
-- foo:1:7-14: Expecting List[String] but having List[Object]
--
-- Reason: You can't merge Lists of different types
--
-- It uses <https://github.com/typesafehub/config Typesafe-config>
-- format: HOCON. HOCON stands for Human-Optimized
-- Config Object Notation. It's basically a JSON superset
--
-- Here's an example:
--
-- > -- app.conf
-- > # This is a comment
-- >
-- > foo.bar = ${toto}
-- >
-- > toto = false
-- >
-- > rawString = """
-- >             This is a multi-
-- >             lines String
-- >             """
-- >
-- > another.string = "I'm a String"
-- >
-- > one.more.string = one more string
-- >
-- > nested {
-- >    list: [ one
-- >          , 1
-- >          , "both"]
-- >
-- >    homing = {
-- >      pass: { b: feez } { a: "Prop"}
-- >    }
-- >
-- >    another: [1,2,3] [4,5,6]
-- > }
--
-- How to use:
--
-- @
-- -- Example.hs
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.Config
-- import Data.Text (Text)
--
-- data Foo = Foo { fooPort :: 'Int', fooAddr :: 'Text' }
--
-- main :: 'IO' ()
-- main = do
--   foo <- loadFooProps
--   withFoo foo
--
--   where
--     loadFooProps = do
--       config <- 'loadConfig' "conf/baz.conf"
--       port   <- 'getInteger' "foo.port" config
--       addr   <- 'getString' "foo.addr" config
--       return (Foo port addr)
--
-- withFoo :: Foo -> 'IO' ()
-- withFoo = ...
-- @
--------------------------------------------------------------------------------
module Data.Config
    ( Config
    , loadConfig
    , getInteger
    , getParsec
    , getString
    , getBool
    , getStrings
    , getBools
    , getIntegers
    , getParsecs
    ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import Data.Typeable

--------------------------------------------------------------------------------
import           Control.Monad.Catch
import qualified Data.Map         as M
import           Data.Text (Text, unpack)
import qualified Data.Text.IO     as T
import           Text.Parsec (Parsec)
import qualified Text.Parsec.Char as Char
import           Text.Parsec.Combinator
import           Text.Parsec.Pos (newPos)
import qualified Text.Parsec.Prim as Prim
import           Text.Parsec.Text ()

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Parser
import Data.Config.Internal.Pos
import Data.Config.Internal.Scoped
import Data.Config.Internal.Reg
import Data.Config.Internal.Register
import Data.Config.Internal.Rename
import Data.Config.Internal.Typecheck
import Data.Config.Internal.Typed

--------------------------------------------------------------------------------
-- | Configuration data
newtype Config = Config { unConf :: Reg }

--------------------------------------------------------------------------------
type Extractor a = forall m. MonadThrow m => Text -> Config -> AST Typed -> m a

--------------------------------------------------------------------------------
newtype ConfigError = ConfigError String deriving Typeable

--------------------------------------------------------------------------------
instance Show ConfigError where
    show (ConfigError s) = s

--------------------------------------------------------------------------------
instance Exception ConfigError

--------------------------------------------------------------------------------
loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Config
loadConfig path
    = do txt <- liftIO $ T.readFile path
         pse <- parse path txt
         let action
                 = do ps         <- pse
                      (tys, pts) <- typecheck $ rename ps
                      return $ register tys pts
         case action of
             Left e    -> throwM e
             Right reg -> return $ Config reg

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
getString :: MonadThrow m => Text -> Config -> m Text
getString key conf = getValue string key conf

--------------------------------------------------------------------------------
getInteger :: MonadThrow m => Text -> Config -> m Integer
getInteger key conf = getParsec integerParsec key conf

--------------------------------------------------------------------------------
getBool :: MonadThrow m => Text -> Config -> m Bool
getBool key conf = getParsec boolParsec key conf

--------------------------------------------------------------------------------
getParsec :: MonadThrow m
          => (forall s. Parsec Text s a)
          -> Text
          -> Config
          -> m a
getParsec action key conf = getValue (parsec action) key conf

--------------------------------------------------------------------------------
getStrings :: MonadThrow m => Text -> Config -> m [Text]
getStrings key conf = getValues string key conf

--------------------------------------------------------------------------------
getIntegers :: MonadThrow m => Text -> Config -> m [Integer]
getIntegers key conf = getParsecs integerParsec key conf

--------------------------------------------------------------------------------
getBools :: MonadThrow m => Text -> Config -> m [Bool]
getBools key conf = getParsecs boolParsec key conf

--------------------------------------------------------------------------------
getParsecs :: MonadThrow m
           => (forall s. Parsec Text s a)
           -> Text
           -> Config
           -> m [a]
getParsecs action key conf = getValues (parsec action) key conf

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
getValue :: MonadThrow m => Extractor a -> Text -> Config -> m a
getValue extr key conf
    = maybe (throwM $ propertyNotFound key) go (M.lookup key reg)
  where
    reg    = regAST $ unConf conf
    go ast = extr key conf (simplify (unConf conf) ast)

--------------------------------------------------------------------------------
getValues :: MonadThrow m => Extractor a -> Text -> Config -> m [a]
getValues extr key conf = getValue (list extr) key conf

--------------------------------------------------------------------------------
string :: Extractor Text
string key _ (AST expr t)
    = case expr of
    ID s     -> return s
    STRING s -> return s
    _        -> throwM (wrongType key pos stringType ty)
  where
    pos = scopePos $ typedScope t
    ty  = typedType t

--------------------------------------------------------------------------------
list :: Extractor a -> Extractor [a]
list extr key conf (AST expr t)
    = case expr of
    LIST xs -> mapM (extr key conf) xs
    _       -> throwM (wrongType key pos someListType ty)
  where
    pos = scopePos $ typedScope t
    ty  = typedType t

--------------------------------------------------------------------------------
integerParsec :: Parsec Text s Integer
integerParsec = fmap read (many1 Char.digit <* eof)

--------------------------------------------------------------------------------
boolParsec :: Parsec Text s Bool
boolParsec
    = (    fmap (const True)  (Char.string "true")
       <|> fmap (const True)  (Char.string "True")
       <|> fmap (const True)  (Char.string "yes")
       <|> fmap (const True)  (Char.string "Yes")
       <|> fmap (const False) (Char.string "false")
       <|> fmap (const False) (Char.string "False")
       <|> fmap (const False) (Char.string "no")
       <|> fmap (const False) (Char.string "No")
       <|> onOff
      ) <* eof
  where
    msg = " when parsing on|off or On|Off"
    onOff
        = do _ <- Char.char 'o' <|> Char.char 'O'
             c <- Char.anyChar
             case c of
                 'n' -> return True
                 'f' -> Char.char 'f' >> return False
                 _   -> Prim.unexpected (show c ++ msg)

--------------------------------------------------------------------------------
parsec :: Parsec Text () a -> Extractor a
parsec action key conf a@(AST _ ty)
    = do s <- string key conf a
         let pos    = scopePos $ typedScope ty
             upd _  = newPos (unpack key) (startLine pos) (startCol pos)
             ini    = Prim.setPosition . upd =<< Prim.getPosition
             err e  = throwM $ ConfigError (ctxStr key pos ++ show e)
             result = Prim.parse (ini >> action) "" s
         either err return result

--------------------------------------------------------------------------------
propertyNotFound :: Text -> ConfigError
propertyNotFound k = ConfigError msg where
  msg = "Property " ++ unpack k ++ " is not found"

--------------------------------------------------------------------------------
wrongType :: Text -> Pos -> Type -> Type -> ConfigError
wrongType key pos tye tyf = ConfigError msg where
  msg = ctxStr key pos ++
        "When accessing, expected " ++ show tye ++
        " but had " ++ show tyf ++ " instead"

--------------------------------------------------------------------------------
ctxStr :: Text -> Pos -> String
ctxStr e pos = unpack e ++ show pos ++ " "
