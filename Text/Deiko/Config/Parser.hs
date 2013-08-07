{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.Deiko.Config.Parser where

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

data LALR s a = Shift a
              | Reduce (Production s) a
              | LookAhead (Token s -> Bool) (Bool -> a)
              | Failure (Token s -> s)
              | PrintStack -- only when debugging

data Cell s = CToken Position (Sym s)
            | CAst (Value s (Ident s))
            | CIdent (Ident s)
            | CProp (Untyped s)
            | CProps [Untyped s]
            | CEOF deriving Show

instance Functor (LALR s) where
  fmap f (Shift a)       = Shift (f a)
  fmap f (Reduce p a)    = Reduce p (f a)
  fmap f (LookAhead p k) = LookAhead p (f . k)
  fmap _ (Failure e)     = Failure e
  fmap _ PrintStack      = PrintStack

type Stack s = [Cell s]

type Production s = Stack s -> (Cell s, Stack s)

identSimple :: Production s
identSimple ((CToken p (ID x)):xs) = (CIdent $ Ident p x, xs)

identSelect :: Production s
identSelect ((CToken p (ID x)):(CToken _ DOT):(CIdent id):xs) =
  (CIdent $ Select id (Ident p x), xs)

stringP :: Production s
stringP ((CToken p (STRING x)):xs) = (CAst $ Mu $ ASTRING p x, xs)
stringP ((CToken p (ID x)):xs)     = (CAst $ Mu $ ASTRING p x, xs)

listP :: Production s
listP ((CToken _ RBRACK):(CToken p LBRACK):xs) = (CAst $ nil p, xs)
listP ((CToken _ RBRACK):(CAst (Mu (ALIST p values))):xs) =
  (CAst $ list p $ reverse values, xs)
listP (x@(CToken _ RBRACK):_:xs) = listP (x:xs)

listHead :: Production s
listHead ((CAst value):(CToken p LBRACK):xs) = (CAst $ Mu $ ALIST p [value], xs)
listHead ((CAst value):(CToken _ COMMA):(CAst (Mu (ALIST p values))):xs) =
  (CAst $ list p (value:values), xs)
listHead (x@(CAst value):y@(CToken _ COMMA):_:xs) = listHead (x:y:xs)
listHead (x@(CAst value):_:xs) = listHead (x:xs)

substP :: Production s
substP ((CToken p (SUBST x)):xs) = (CAst $ subst p x, xs)

mergeP :: Production s
mergeP ((CAst y):(CToken _ SPACE):(CAst x):xs) = (CAst $ merge x y, xs)
mergeP (_:xs) = mergeP xs

objectP :: Production s
objectP ((CToken _ RBRACE):(CToken p LBRACE):xs) = (CAst $ object p [], xs)
objectP ((CToken _ RBRACE):(CProps ps):(CToken p LBRACE):xs) =
  (CAst $ object p $ reverse ps, xs)
objectP (x@(CToken _ RBRACE):y@(CProps _):_:xs) = objectP (x:y:xs)
objectP (x@(CToken _ RBRACE):_:xs) = objectP (x:xs)

propertyP :: Production s
propertyP (x@(CAst _):y@(CIdent _):(CToken _ SPACE):xs) = propertyP (x:y:xs)
propertyP ((CAst value):(CIdent id):xs) = (CProp $ property id value, xs)
propertyP (x@(CAst value):_:xs) = propertyP (x:xs)
propertyP (_:xs) = propertyP xs -- trailling space, ex: id: value_[end]

propertiesHead :: Production s
propertiesHead ((CProp p):xs) = (CProps [p], xs)

properties :: Production s
properties ((CProp p):(CProps ps):xs)          = (CProps (p:ps), xs)
properties (x@(CProp _):(CToken _ _):xs)        = properties (x:xs)
properties ((CToken _ NEWLINE):(CProps ps):xs) = (CProps ps, xs)
properties (_:xs)                              = properties xs

shift :: F (LALR s) ()
shift = wrap $ Shift (return ())

reduce :: Production s -> F (LALR s) ()
reduce prod = wrap $ Reduce prod (return ())

lookAhead :: (Token s -> Bool) -> F (LALR s) Bool
lookAhead p = wrap $ LookAhead p return

printStack :: F (LALR s) a
printStack = wrap PrintStack

failure :: (Token s -> s) -> F (LALR s) a
failure k = wrap $ Failure k

isId :: Token s -> Bool
isId (Elm _ _ (ID _)) = True
isId _                = False

isDot :: Token s -> Bool
isDot (Elm _ _ DOT) = True
isDot _             = False

isLBrack :: Token s -> Bool
isLBrack (Elm _ _ LBRACK) = True
isLBrack _                = False

isRBrack :: Token s -> Bool
isRBrack (Elm _ _ RBRACK) = True
isRBrack _                = False

isLBrace :: Token s -> Bool
isLBrace (Elm _ _ LBRACE) = True
isLBrace _                = False

isRBrace :: Token s -> Bool
isRBrace (Elm _ _ RBRACE) = True
isRBrace _                = False

isString :: Token s -> Bool
isString (Elm _ _ (STRING _)) = True
isString _                    = False

isSubst :: Token s -> Bool
isSubst (Elm _ _ (SUBST _)) = True
isSubst _                   = False

isComma :: Token s -> Bool
isComma (Elm _ _ COMMA) = True
isComma _               = False

isSpace :: Token s -> Bool
isSpace (Elm _ _ SPACE) = True
isSpace _               = False

isNewline :: Token s -> Bool
isNewline (Elm _ _ NEWLINE) = True
isNewline _                 = False

isEqual :: Token s -> Bool
isEqual (Elm _ _ EQUAL) = True
isEqual _               = False

isEOF :: Token s -> Bool
isEOF EOF = True
isEOF _   = False

anything :: Token s -> Bool
anything _ = True

shiftSpace :: F (LALR s) ()
shiftSpace =
  alt [(isSpace, shift)
      ,(anything, return ())]

shiftNewline :: F (LALR s) ()
shiftNewline =
  alt [(isNewline, shift)
      ,(anything, return ())]

shiftSpaceOrNewline :: F (LALR s) ()
shiftSpaceOrNewline = shiftSpace >> shiftNewline

parseId :: StringLike s => F (LALR s) ()
parseId = do
  shift
  reduce identSimple
  alt [(isDot, parseSelect)
      ,(anything, return ())]

parseSelect :: StringLike s => F (LALR s) ()
parseSelect = do
  shift
  alt [(isId, shift >> go)
      ,(anything, failure unexpected)]

    where
      go = do
        reduce identSelect
        dot <- lookAhead isDot
        when dot parseSelect

parseString :: F (LALR s) ()
parseString = shift >> reduce stringP

parseProperty :: StringLike s => F (LALR s) ()
parseProperty = do
  shiftSpace
  alt [(isId, parseId >> go)
      ,(anything, failure unexpected)]

  where
    go = do
      shiftSpace
      alt [(isEqual, shift >> step1)
          ,(isLBrace, parseObject)
          ,(anything, failure unexpected)]
      reduce propertyP

    step1 = shiftSpace >> parseValue

parseProperties :: StringLike s => F (LALR s) ()
parseProperties = do
  shiftSpaceOrNewline
  parseProperty
  reduce propertiesHead
  shiftSpace
  go

  where
    go =
      alt [(isComma, reduction)
          ,(isNewline, shift >> inter)
          ,(anything, return ())]

    inter = do
      shiftSpaceOrNewline
      alt [(isEOF, reduce properties)
          ,(isRBrace, reduce properties)
          ,(anything, reduction)]

    reduction = do
      parseProperty
      reduce properties
      go

parseObject :: StringLike s => F (LALR s) ()
parseObject = do
  shift
  shiftSpaceOrNewline
  alt [(isRBrace, shift >> reduce objectP)
      ,(anything, parseProperties >> end)]

  where
    end =
      alt [(isRBrace, shift >> reduce objectP)
          ,(anything, failure unexpected)]

parseList :: StringLike s => F (LALR s) ()
parseList = do
  shift
  shiftSpaceOrNewline
  alt [(isRBrack, shift >> reduce listP)
      ,(anything, go)]

  where
    go = do
      parseListHead
      shiftSpaceOrNewline
      alt [(isRBrack, shift >> reduce listP)
          ,(anything, failure unexpected)]

parseListHead :: StringLike s => F (LALR s) ()
parseListHead = do
  parseValue
  reduce listHead
  go
    where
      go = do
        shiftSpaceOrNewline
        shiftSpace -- sometimes having leading space after a newline
        alt [(isComma, step)
            ,(isRBrack, return ())
            ,(anything, failure unexpected)]

      step = shift >> shiftSpaceOrNewline >> parseValue >> reduce listHead >> go

parseMerge :: StringLike s => F (LALR s) ()
parseMerge = do
  shift
  alt [(isRBrack, return ())
      ,(isRBrace, return ())
      ,(isEOF, return ())
      ,(isNewline, return ())
      ,(anything, go)]

  where
    go = do
      parseValue
      reduce mergeP
      alt [(isSpace, parseMerge)
          ,(anything, return ())]

parseValue :: StringLike s => F (LALR s) ()
parseValue = do
  alt [(isString, parseString)
      ,(isId, parseString)
      ,(isSubst, shift >> reduce substP)
      ,(isLBrack, parseList)
      ,(isLBrace, parseObject)
      ,(anything, failure unexpected)]
  alt [(isSpace, parseMerge)
      ,(anything, return ())]

alt :: [(Token s -> Bool, F (LALR s) ())] -> F (LALR s) ()
alt []               = return ()
alt ((f, action):xs) = lookAhead f >>= go
  where
    go isF
      | isF       = action
      | otherwise = alt xs

recv :: Monad m => (Token s -> Conduit (Token s) m a) -> Conduit (Token s) m a
recv k = await >>= \t -> maybe (error "Exhausted source") k t

toCell :: Token s -> Cell s
toCell (Elm l c s) = CToken (l, c) s
toCell EOF         = CEOF

makeParser :: (Monad m, IsString s, Show s)
           => Free (LALR s) ()
           -> Conduit (Token s) (ErrorT (ConfigError s) m) [Untyped s]
makeParser instr = (cataFree pure impure instr) ([], Nothing)
  where
    pure _ (((CProps xs):_),_) = yield (reverse xs)

    impure (Shift k)       = shifting k
    impure (Reduce p k)    = reducing p k
    impure (LookAhead p k) = looking p k
    impure (Failure k)     = failing k
    impure PrintStack      = reporting

    shifting k (stack, ahead) = maybe (recv go) go ahead
      where
        go t = k (toCell t:stack, Nothing)

    reducing p k (stack, ahead) =
      let (prod, stack1) = p stack in k ((prod:stack1), ahead)

    looking p k (stack, ahead) = maybe (recv go) go ahead
      where
        go h = k (p h) (stack, Just h)

    failing k (_, (Just h)) = lift $ throwError (ConfigError $ k h)

    reporting (stack, _) = error $ show stack

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
