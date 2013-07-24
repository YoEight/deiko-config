{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Text.Deiko.Config.Parser where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Free (Free, wrap)
import Control.Monad.Free.Church (F, fromF)
import Data.Conduit
import Data.Foldable (traverse_, foldMap)
import Text.Deiko.Config.Internal
import Text.Deiko.Config.Util

data LALR a = Shift a
            | Reduce Production a
            | LookAhead (Token -> Bool) (Bool -> a)
            | Failure (Token -> String)
            | PrintStack -- only when debugging

data Cell = CToken Position Sym
          | CAst (Mu (AST Ident))
          | CIdent Ident
          | CProp (Property Ident)
          | CProps [Property Ident]
          | CEOF deriving Show

instance Functor LALR where
  fmap f (Shift a)       = Shift (f a)
  fmap f (Reduce p a)    = Reduce p (f a)
  fmap f (LookAhead p k) = LookAhead p (f . k)
  fmap _ (Failure e)     = Failure e
  fmap _ PrintStack      = PrintStack

type Stack = [Cell]

type Production = Stack -> (Cell, Stack)

type Transformation m = 
  (Stack, Maybe Token) -> Conduit Token (ErrorT ConfigError m) [Property Ident]

identSimple :: Production
identSimple ((CToken p (ID x)):xs) = (CIdent $ Ident p x, xs)

identSelect :: Production
identSelect ((CToken p (ID x)):(CToken _ DOT):(CIdent id):xs) =
  (CIdent $ Select id (Ident p x), xs)

stringP :: Production
stringP ((CToken p (STRING x)):xs) = (CAst $ Mu $ ASTRING p x, xs)
stringP ((CToken p (ID x)):xs)     = (CAst $ Mu $ ASTRING p x, xs) 

listP :: Production
listP ((CToken _ RBRACK):(CToken p LBRACK):xs) = (CAst $ nil p, xs)
listP ((CToken _ RBRACK):(CAst (Mu (ALIST p values))):xs) = 
  (CAst $ list p $ reverse values, xs)
listP (x@(CToken _ RBRACK):_:xs) = listP (x:xs)

listHead :: Production
listHead ((CAst value):(CToken p LBRACK):xs) = (CAst $ Mu $ ALIST p [value], xs)
listHead ((CAst value):(CToken _ COMMA):(CAst (Mu (ALIST p values))):xs) =
  (CAst $ list p (value:values), xs)
listHead (x@(CAst value):y@(CToken _ COMMA):_:xs) = listHead (x:y:xs) 
listHead (x@(CAst value):_:xs) = listHead (x:xs)

substP :: Production
substP ((CToken p (SUBST x)):xs) = (CAst $ subst p x, xs)

mergeP :: Production
mergeP ((CAst y):(CToken _ SPACE):(CAst x):xs) = (CAst $ merge x y, xs)
mergeP (_:xs) = mergeP xs

objectP :: Production
objectP ((CToken _ RBRACE):(CToken p LBRACE):xs) = (CAst $ object p [], xs)
objectP ((CToken _ RBRACE):(CProps ps):(CToken p LBRACE):xs) =
  (CAst $ object p $ reverse ps, xs)
objectP (x@(CToken _ RBRACE):y@(CProps _):_:xs) = objectP (x:y:xs)
objectP (x@(CToken _ RBRACE):_:xs) = objectP (x:xs)

propertyP :: Production
propertyP (x@(CAst _):y@(CIdent _):(CToken _ SPACE):xs) = propertyP (x:y:xs)
propertyP ((CAst value):(CIdent id):xs) = (CProp $ property id value, xs)
propertyP (x@(CAst value):_:xs) = propertyP (x:xs)
propertyP (_:xs) = propertyP xs -- trailling space, ex: id: value_[end]

propertiesHead :: Production
propertiesHead ((CProp p):xs) = (CProps [p], xs)

properties :: Production
properties ((CProp p):(CProps ps):xs)          = (CProps (p:ps), xs)
properties (x@(CProp _):(CToken _ _):xs)        = properties (x:xs)
properties ((CToken _ NEWLINE):(CProps ps):xs) = (CProps ps, xs)
properties (_:xs)                              = properties xs

shift :: F LALR ()
shift = wrap $ Shift (return ())

reduce :: Production -> F LALR ()
reduce prod = wrap $ Reduce prod (return ())

lookAhead :: (Token -> Bool) -> F LALR Bool
lookAhead p = wrap $ LookAhead p return

printStack :: F LALR a
printStack = wrap PrintStack

failure :: (Token -> String) -> F LALR a
failure k = wrap $ Failure k

isId :: Token -> Bool
isId (Elm _ _ (ID _)) = True
isId _                = False

isDot :: Token -> Bool
isDot (Elm _ _ DOT) = True
isDot _             = False

isLBrack :: Token -> Bool
isLBrack (Elm _ _ LBRACK) = True
isLBrack _                = False

isRBrack :: Token -> Bool
isRBrack (Elm _ _ RBRACK) = True
isRBrack _                = False

isLBrace :: Token -> Bool
isLBrace (Elm _ _ LBRACE) = True
isLBrace _                = False

isRBrace :: Token -> Bool
isRBrace (Elm _ _ RBRACE) = True
isRBrace _                = False

isString :: Token -> Bool
isString (Elm _ _ (STRING _)) = True
isString _                    = False

isSubst :: Token -> Bool
isSubst (Elm _ _ (SUBST _)) = True
isSubst _                   = False

isComma :: Token -> Bool
isComma (Elm _ _ COMMA) = True
isComma _               = False

isSpace :: Token -> Bool
isSpace (Elm _ _ SPACE) = True
isSpace _               = False

isNewline :: Token -> Bool
isNewline (Elm _ _ NEWLINE) = True
isNewline _                 = False

isEqual :: Token -> Bool
isEqual (Elm _ _ EQUAL) = True
isEqual _               = False

isEOF :: Token -> Bool
isEOF EOF = True
isEOF _   = False

anything :: Token -> Bool
anything _ = True

shiftSpace :: F LALR ()
shiftSpace =
  alt [(isSpace, shift)
      ,(anything, return ())]

shiftNewline :: F LALR ()
shiftNewline =
  alt [(isNewline, shift)
      ,(anything, return ())]

shiftSpaceOrNewline :: F LALR ()
shiftSpaceOrNewline = shiftSpace >> shiftNewline

parseId :: F LALR ()
parseId = do
  shift
  reduce identSimple
  alt [(isDot, parseSelect)
      ,(anything, return ())]

parseSelect :: F LALR ()
parseSelect = do
  shift
  alt [(isId, shift >> go)
      ,(anything, failure unexpected)]

    where
      go = do
        reduce identSelect
        dot <- lookAhead isDot
        when dot parseSelect

parseString :: F LALR ()
parseString = shift >> reduce stringP

parseProperty :: F LALR ()
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

parseProperties :: F LALR ()
parseProperties = do
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

parseObject :: F LALR ()
parseObject = do
  shift
  shiftSpaceOrNewline
  alt [(isRBrace, shift >> reduce objectP)
      ,(anything, parseProperties >> end)]
    
  where
    end =
      alt [(isRBrace, shift >> reduce objectP)
          ,(anything, failure unexpected)]

parseList :: F LALR ()
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

parseListHead :: F LALR ()
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

parseMerge :: F LALR ()
parseMerge = do
  shift
  alt [(isRBrack, return ())
      ,(isRBrace, return ())
      ,(isEOF, return ())
      ,(anything, go)]

  where
    go = do
      parseValue
      reduce mergeP
      alt [(isSpace, parseMerge)
          ,(anything, return ())]

parseValue :: F LALR ()
parseValue = do
  alt [(isString, parseString)
      ,(isId, parseString)
      ,(isSubst, shift >> reduce substP)
      ,(isLBrack, parseList)
      ,(isLBrace, parseObject)
      ,(anything, failure unexpected)]
  alt [(isSpace, parseMerge)
      ,(anything, return ())]

alt :: [(Token -> Bool, F LALR ())] -> F LALR ()
alt []               = return ()
alt ((f, action):xs) = lookAhead f >>= go
  where
    go isF 
      | isF       = action
      | otherwise = alt xs 

recv :: Monad m => (Token -> Conduit Token m a) -> Conduit Token m a
recv k = await >>= \t -> maybe (error "Exhausted source") k t

toCell :: Token -> Cell
toCell (Elm l c s) = CToken (l, c) s
toCell EOF         = CEOF

makeParser :: Monad m 
           => Free LALR () 
           -> Conduit Token (ErrorT ConfigError m) [Property Ident]
makeParser instr = (cataFree pure impure instr) ([], Nothing)
  where
    pure _ (((CProps xs):_),_) = yield (reverse xs)

    impure (Shift k)       = shifting k
    impure (Reduce p k)    = reducing p k
    impure (LookAhead p k) = looking p k
    impure (Failure k)     = failing k
    impure PrintStack      = reporting

shifting :: Monad m => Transformation m -> Transformation m
shifting k (stack, ahead) = maybe (recv go) go ahead
  where
    go t = k (toCell t:stack, Nothing)

reducing :: Production -> Transformation m -> Transformation m
reducing p k (stack, ahead) = 
  let (prod, stack1) = p stack in k ((prod:stack1), ahead)

looking :: Monad m 
        => (Token -> Bool) 
        -> (Bool -> Transformation m) 
        -> Transformation m
looking p k (stack, ahead) = maybe (recv go) go ahead
  where 
    go h = k (p h) (stack, Just h)

failing :: Monad m => (Token -> String) -> Transformation m
failing k (_, (Just h)) = lift $ throwError (ConfigError $ k h) 

reporting :: Transformation m 
reporting (stack, _) = error $ show stack

unexpected :: Token -> String
unexpected (Elm l c sym) = 
  "Unexpected token " ++ show sym ++ " at (" ++ show l ++ ", " ++ show c ++ ")" 

parser :: Monad m 
       => Conduit Token (ErrorT ConfigError m) [Property Ident]
parser = makeParser (fromF parseProperties)
