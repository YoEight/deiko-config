module Text.Deiko.Config.Parser where

import Text.Deiko.Config.Lexer
import Text.Deiko.Config.Util
import Control.Monad
import Data.Conduit
import Data.Foldable (traverse_)

data LALR a = Shift a
            | Reduce Production a
            | LookAhead (Token -> Bool) (Bool -> a)
            | Failure (Token -> String)
            | PrintStack -- only when debugging

data AST a = ASTRING String
           | ALIST [a]
           | ASUBST String
           | AMERGE a a
           | AOBJECT [a]
           | ASIMPLE String
           | ASELECT a a
           | APROP a a deriving Show

data Cell = CToken Int Int Sym
          | CAst (Mu AST)
          | CEOF

instance Functor AST where
  fmap f (ASTRING x)   = ASTRING x
  fmap f (ALIST xs)    = ALIST (fmap f xs)
  fmap f (ASUBST x)    = ASUBST x
  fmap f (AMERGE x y)  = AMERGE (f x) (f y)
  fmap f (AOBJECT xs)  = AOBJECT (fmap f xs)
  fmap f (ASIMPLE x)   = ASIMPLE x
  fmap f (ASELECT x y) = ASELECT (f x) (f y)
  fmap f (APROP x y)   = APROP (f x) (f y)

instance Functor LALR where
  fmap f (Shift a)       = Shift (f a)
  fmap f (Reduce p a)    = Reduce p (f a)
  fmap f (LookAhead p k) = LookAhead p (f . k)
  fmap _ (Failure e)     = Failure e
  fmap _ PrintStack      = PrintStack

type Stack = [Cell]

type Production = Stack -> (Mu AST, Stack)

type Transformation m = 
  (Stack, Maybe Token) -> Sink Token m (Either String (Mu AST))

identSimple :: Production
identSimple ((CToken _ _ (ID x)):xs) = (Mu $ ASIMPLE x, xs)

identSelect :: Production
identSelect ((CToken _ _ (ID x)):(CToken _ _ DOT):(CAst mu):xs) =
  (Mu $ ASELECT mu (Mu $ ASIMPLE x), xs)

string :: Production
string ((CToken _ _ (STRING x)):xs) = (Mu $ ASTRING x, xs)
string ((CToken _ _ (ID x)):xs)     = (Mu $ ASTRING x, xs) 

identValue :: Production
identValue ((CAst (Mu (ASIMPLE x))):xs) = (Mu $ ASTRING x, xs)

list :: Production
list ((CToken _ _ RBRACK):(CToken _ _ LBRACK):xs) = (Mu $ ALIST [], xs)
list ((CToken _ _ RBRACK):(CAst (Mu (ALIST values))):xs) = 
  (Mu $ ALIST $ reverse values, xs)
list (x@(CToken _ _ RBRACK):_:xs) = list (x:xs)

listHead :: Production
listHead ((CAst value):(CToken _ _ LBRACK):xs) = (Mu $ ALIST [value], xs)
listHead ((CAst value):(CToken _ _ COMMA):(CAst (Mu (ALIST values))):xs) =
  (Mu $ ALIST (value:values), xs)
listHead (x@(CAst value):y@(CToken _ _ COMMA):_:xs) = listHead (x:y:xs) 
listHead (x@(CAst value):_:xs) = listHead (x:xs)

subst :: Production
subst ((CToken _ _ (SUBST x)):xs) = (Mu $ ASUBST x, xs)

merge :: Production
merge ((CAst y):(CToken _ _ SPACE):(CAst x):xs) = (Mu $ AMERGE x y, xs)
merge (_:xs) = merge xs

object :: Production
object ((CToken _ _ RBRACE):(CToken _ _ LBRACE):xs) = (Mu $ AOBJECT [], xs)
object ((CToken _ _ RBRACE):(CAst (Mu (ALIST ps))):(CToken _ _ LBRACE):xs) =
  (Mu $ AOBJECT $ reverse ps, xs)
object (x@(CToken _ _ RBRACE):y@(CAst (Mu (ALIST _))):_:xs) = object (x:y:xs)
object (x@(CToken _ _ RBRACE):_:xs) = object (x:xs)

property :: Production
property (x@(CAst value):y@(CAst ident):(CToken _ _ SPACE):xs) = 
  property (x:y:xs)
property ((CAst value):(CAst ident):xs) = (Mu $ APROP ident value, xs)
property (x@(CAst value):_:xs) = property (x:xs)
property (_:xs) = property xs -- trailling space, ex: id: value_[end]

propertiesHead :: Production
propertiesHead ((CAst prop):xs) = (Mu $ ALIST [prop], xs)

properties :: Production
properties ((CAst prop):(CAst (Mu (ALIST props))):xs) = 
  (Mu $ ALIST (prop:props), xs)
properties (x@(CAst _):(CToken _ _ _):xs) = properties (x:xs)
properties ((CToken _ _ NEWLINE):(CAst (Mu (ALIST props))):xs) =
  (Mu $ ALIST props, xs)
properties (_:xs) = properties xs

shift :: Free LALR ()
shift = Suspend $ Shift (Return ())

reduce :: Production -> Free LALR ()
reduce prod = Suspend $ Reduce prod (Return ())

lookAhead :: (Token -> Bool) -> Free LALR Bool
lookAhead p = Suspend $ LookAhead p Return

showStack :: Free LALR a
showStack = Suspend PrintStack

failure :: (Token -> String) -> Free LALR a
failure k = Suspend $ Failure k

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

shiftSpace :: Free LALR ()
shiftSpace =
  alt [(isSpace, shift)
      ,(anything, return ())]

shiftNewline :: Free LALR ()
shiftNewline =
  alt [(isNewline, shift)
      ,(anything, return ())]

shiftSpaceOrNewline :: Free LALR ()
shiftSpaceOrNewline = shiftSpace >> shiftNewline

parseId :: Free LALR ()
parseId = do
  shift
  reduce identSimple
  alt [(isDot, parseSelect)
      ,(anything, return ())]

parseSelect :: Free LALR ()
parseSelect = do
  shift
  alt [(isId, shift >> go)
      ,(anything, failure unexpected)]

    where
      go = do
        reduce identSelect
        dot <- lookAhead isDot
        when dot parseSelect

parseString :: Free LALR ()
parseString = shift >> reduce string

parseProperty :: Free LALR ()
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
      reduce property
    
    step1 = shiftSpace >> parseValue

parseProperties :: Free LALR ()
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
      shiftSpace
      reduce properties
      go

parseObject :: Free LALR ()
parseObject = do
  shift
  shiftSpaceOrNewline
  alt [(isRBrace, shift >> reduce object)
      ,(anything, parseProperties >> end)]
    
  where
    end =
      alt [(isRBrace, shift >> reduce object)
          ,(anything, failure unexpected)]

parseList :: Free LALR ()
parseList = do 
  shift
  shiftSpaceOrNewline
  alt [(isRBrack, shift >> reduce list)
      ,(anything, go)]

  where
    go = do
      parseListHead
      shiftSpaceOrNewline
      alt [(isRBrack, shift >> reduce list)
          ,(anything, failure unexpected)]

parseListHead :: Free LALR ()
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

parseMerge :: Free LALR ()
parseMerge = do
  shift
  alt [(isRBrack, return ())
      ,(isRBrace, return ())
      ,(isEOF, return ())
      ,(anything, go)]

  where
    go = do
      parseValue
      reduce merge
      alt [(isSpace, parseMerge)
          ,(anything, return ())]

parseValue :: Free LALR ()
parseValue = do
  alt [(isString, parseString)
      ,(isId, parseString)
      ,(isSubst, shift >> reduce subst)
      ,(isLBrack, parseList)
      ,(isLBrace, parseObject)
      ,(anything, failure unexpected)]
  alt [(isSpace, parseMerge)
      ,(anything, return ())]

alt :: [(Token -> Bool, Free LALR ())] -> Free LALR ()
alt []               = return ()
alt ((f, action):xs) = lookAhead f >>= go
  where
    go isF 
      | isF       = action
      | otherwise = alt xs 

recv :: Monad m => (Token -> Sink Token m a) -> Sink Token m a
recv k = await >>= \t -> maybe (error "Exhausted source") k t

toCell :: Token -> Cell
toCell (Elm c l s) = CToken c l s
toCell EOF         = CEOF

makeParser :: Monad m => Free LALR () -> Sink Token m (Either String (Mu AST))
makeParser instr = (cataFree pure impure instr) ([], Nothing)
  where
    pure _ ((CAst (Mu (ALIST xs))):_,_) = 
      return (Right $ Mu $ ALIST (reverse xs))
    
    impure (Shift k)       = shifting k
    impure (Reduce p k)    = reducing p k
    impure (LookAhead p k) = looking p k
    impure (Failure k)     = failing k
    impure PrintStack      = reporting

shifting :: Monad m => Transformation m -> Transformation m
shifting k (stack, ahead) = maybe (recv go) go ahead
  where
    go t = k (toCell t:stack, Nothing)

reducing :: Monad m => Production -> Transformation m -> Transformation m
reducing p k (stack, ahead) = 
  let (prod, stack1) = p stack
      stack2         = (CAst prod:stack1) in 
  k (stack2, ahead)

looking :: Monad m 
        => (Token -> Bool)
        -> (Bool -> Transformation m)
        -> Transformation m
looking p k (stack, ahead) = maybe (recv go) go ahead
  where 
    go h = k (p h) (stack, Just h)

failing :: Monad m => (Token -> String) -> Transformation m
failing k (_, (Just h)) = return $ Left (k h) 

reporting :: Monad m => Transformation m 
reporting (stack, _) = error $ printStack stack

printStack :: Stack -> String
printStack [] = "*empty stack*"
printStack xs = foldr1 (\x y -> x ++ ", " ++ y) (fmap go xs)
  where
    go (CToken _ _ sym) = show sym
    go CEOF             = "$"
    go (CAst mu)  = cata toString mu
    
    toString (APROP i v)   = "prop(" ++ i ++ ":" ++ v ++ ")"
    toString (ASIMPLE x)   = "id(" ++ x ++ ")"
    toString (ASELECT x y) = x ++ "." ++ y 
    toString (ASTRING x)   = "string(" ++ x ++ ")"
    toString (ALIST _)     =  "list"

unexpected :: Token -> String
unexpected (Elm l c sym) = 
  "Unexpected token " ++ show sym ++ " at (" ++ show l ++ ", " ++ show c ++ ")" 

printer :: Either String (Mu AST) -> IO ()
printer = print . either id (cata go) 
  where
    go (ASTRING x)   = "string(" ++ x ++ ")"
    go (ASUBST x)    = "${" ++ x ++ "}"
    go (AMERGE x y)  = "merge(" ++ x ++ " =:= " ++ y ++ ")"
    go (APROP i v)   = i ++ ": " ++ v
    go (AOBJECT xs)
      | null xs      = "object{}"
      | otherwise    = "object{" ++ foldr1 (\x y -> x ++ "," ++ y) xs ++ "}"
    go (ALIST xs)
      | null xs      = "list([])"
      | otherwise    = "list([" ++ foldr1 (\x y -> x ++ "," ++ y) xs ++ "])"
    go (ASIMPLE x)   = x
    go (ASELECT i v) = i ++ "." ++ v

parser :: Monad m => Sink Token m (Either String (Mu AST))
parser = makeParser parseProperties
