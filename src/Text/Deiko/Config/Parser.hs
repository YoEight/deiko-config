{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.Deiko.Config.Parser where

import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Control.Monad.Error
import Control.Monad.Free (Free, wrap)
import Control.Monad.Free.Church (F, fromF)
import Data.Conduit
import Data.Foldable (traverse_, foldMap)
import Data.Monoid (Monoid, (<>))
import Data.String (IsString (..))
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

data LALR s a = Shift (Token s -> a)
              | LookAhead (Token s -> a)
              | Failure s

instance Functor (LALR s) where
  fmap f (Shift k)     = Shift (f . k)
  fmap f (LookAhead k) = LookAhead (f . k)
  fmap _ (Failure e)   = Failure e

shift :: F (LALR s) (Token s)
shift = wrap $ Shift return

shift_ :: F (LALR s) ()
shift_ = void shift

lookAhead :: F (LALR s) (Token s)
lookAhead = wrap $ LookAhead return

failure :: s -> F (LALR s) a
failure e = wrap $ Failure e

withLook :: (Token s -> F (LALR s) a) -> F (LALR s) a
withLook k = lookAhead >>= k

withShift :: (Token s -> F (LALR s) a) -> F (LALR s) a
withShift k = shift >>= k

shiftSpace :: F (LALR s) ()
shiftSpace = withLook go
  where
    go (Elm _ _ SPACE) = shift_
    go _              = return ()

shiftNewline :: F (LALR s) ()
shiftNewline = withLook go
  where
    go (Elm _ _ NEWLINE) = shift_
    go _                 = return ()

shiftSpaceOrNewline :: F (LALR s) ()
shiftSpaceOrNewline = shiftSpace >> shiftNewline

parseId :: StringLike s => F (LALR s) (Ident s)
parseId = do
  t <- shift
  case t of
    (Elm l c (ID s))     -> return (Ident (l,c) s)
    (Elm l c (STRING s)) -> return (Ident (l,c) s)
    _                    -> failure (unexpected t)

parseSelect :: StringLike s => F (LALR s) (Ident s)
parseSelect = do
  id <- parseId
  loop id
    where
      loop id = do
        t <- lookAhead
        case t of
          (Elm _ _ DOT) -> shift_ >> fmap (Select id) parseSelect
          _             -> return id

parseString :: F (LALR s) (Value s (Ident s))
parseString = do
  t <- shift
  case t of
    (Elm l c (ID s))     -> return $ Mu $ ASTRING (l,c) s
    (Elm l c (STRING s)) -> return $ Mu $ ASTRING (l,c) s

parseProperty :: StringLike s => F (LALR s) (Untyped s)
parseProperty = do
  shiftSpace
  id <- parseSelect
  shiftSpace
  t <- lookAhead
  case t of
    (Elm _ _ EQUAL)  -> onEqual id
    (Elm _ _ LBRACE) -> fmap (Prop id) parseObject
    _                -> failure (unexpected t)
  where
    onEqual id = do
      shift
      shiftSpace
      v <- parseValue
      return $ Prop id v

parseProperties :: StringLike s => F (LALR s) [Untyped s]
parseProperties = do
  shiftSpaceOrNewline
  prop <- parseProperty
  shiftSpace
  loop prop
    where
      loop x =
        lookAhead >>= \t ->
          case t of
            (Elm _ _ COMMA)   -> onComma x
            (Elm _ _ NEWLINE) -> onNewline x
            _                 -> return [x]

      onComma x = do
        shift
        fmap (x:) parseProperties

      onNewline x = do
        shiftSpaceOrNewline
        shiftSpace -- sometimes having leading space after a newline
        t2 <- lookAhead
        case t2 of
          (Elm _ _ RBRACE) -> return [x]
          EOF              -> return [x]
          _                -> fmap (x:) parseProperties

parseObject :: StringLike s => F (LALR s) (Value s (Ident s))
parseObject = do
  (Elm l c LBRACE) <- shift
  shiftSpaceOrNewline
  t <- lookAhead
  case t of
    (Elm _ _ RBRACE) -> shift >> return (Mu $ AOBJECT (l,c) [])
    _  -> do
      xs <- parseProperties
      d  <- shift
      case d of
        (Elm _ _ RBRACE) -> return $ Mu $ AOBJECT (l,c) xs
        _                -> failure (unexpected d)

parseList :: StringLike s => F (LALR s) (Value s (Ident s))
parseList = do
  (Elm l c LBRACK) <- shift
  shiftSpaceOrNewline
  t <- lookAhead
  case t of
    (Elm _ _ RBRACK) -> shift >> (return $ Mu $ ALIST (l,c) [])
    _                -> reduction (l,c) <$> parseValue <*> loop []
  where
    reduction p x xs = Mu $ ALIST p (x:xs)
    loop xs = do
      shiftSpaceOrNewline
      shiftSpace -- sometimes having leading space after a newline
      t <- shift
      case t of
        (Elm _ _ RBRACK) -> return (reverse xs)
        (Elm _ _ COMMA)  -> onComma xs
        _                -> failure (unexpected t)

    onComma xs = do
      shiftSpaceOrNewline
      parseValue >>= (loop . (:xs))

parseSubst :: F (LALR s) (Value s (Ident s))
parseSubst = withShift (return . go)
  where
    go (Elm l c (SUBST s)) = Mu $ ASUBST (l,c) s

parseValue :: StringLike s => F (LALR s) (Value s (Ident s))
parseValue = loop =<< withLook go
  where
    go (Elm _ _ (ID _))     = parseString
    go (Elm _ _ (STRING _)) = parseString
    go (Elm _ _ (SUBST _))  = parseSubst
    go (Elm _ _ LBRACK)     = parseList
    go (Elm _ _ LBRACE)     = parseObject
    go x                    = failure (unexpected x)

    loop v = do
      t <- lookAhead
      case t of
        (Elm _ _ SPACE) -> onMergeSpace v
        _               -> return v

    onMergeSpace v = do
      shift
      t <- lookAhead
      case t of
        (Elm _ _ RBRACE) -> return v
        (Elm _ _ RBRACK) -> return v
        _                -> fmap (Mu . AMERGE v) parseValue

recv :: Monad m => (Token s -> Conduit (Token s) m a) -> Conduit (Token s) m a
recv k = await >>= \t -> maybe (error "Exhausted source") k t

makeParser :: (Monad m, IsString s, Show s)
           => Free (LALR s) a
           -> Conduit (Token s) (ErrorT (ConfigError s) m) a
makeParser instr = cataFree pure impure instr $ Nothing
  where
    pure a _ = yield a

    impure (Shift k)     = shifting k
    impure (LookAhead k) = looking k
    impure (Failure e)   = failing e

    shifting k ahead = maybe (recv go) go ahead
      where
        go t = k t $ Nothing

    looking k ahead = maybe (recv go) go ahead
      where
        go t = k t $ (Just t)

    failing e _ = lift $ throwError (ConfigError e)

unexpected :: (IsString s, Monoid s, Show s) => Token s -> s
unexpected (Elm l c sym) =
  "Unexpected token " <> symStr <> " at (" <> line <> ", " <> col <> ")"
    where
      symStr = fromString $ show sym
      line   = fromString $ show l
      col    = fromString $ show c

parser :: (Monad m, StringLike s)
       => Conduit (Token s) (ErrorT (ConfigError s) m) [Untyped s]
parser = makeParser (fromF parseProperties)
