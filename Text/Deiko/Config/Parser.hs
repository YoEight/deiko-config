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

type Position = (Int, Int)

data AST a = ASTRING Position String
           | ALIST Position [a]
           | ASUBST Position String
           | AMERGE a a
           | AOBJECT Position [a]
           | APROP a a deriving Show

data Cell = CToken Position Sym
          | CAst (Mu AST)
          | CEOF

instance Functor AST where
  fmap f (ASTRING p x)   = ASTRING p x
  fmap f (ALIST p xs)    = ALIST p (fmap f xs)
  fmap f (ASUBST p x)    = ASUBST p x
  fmap f (AMERGE x y)    = AMERGE (f x) (f y)
  fmap f (AOBJECT p xs)  = AOBJECT p (fmap f xs)
  fmap f (APROP x y)     = APROP (f x) (f y)

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
identSimple ((CToken p (ID x)):xs) = (Mu $ ASTRING p x, xs)

identSelect :: Production
identSelect ((CToken p (ID x)):(CToken _ DOT):(CAst mu):xs) =
  (Mu $ AMERGE mu (Mu $ ASTRING p x), xs)

string :: Production
string ((CToken p (STRING x)):xs) = (Mu $ ASTRING p x, xs)
string ((CToken p (ID x)):xs)     = (Mu $ ASTRING p x, xs) 

--identValue :: Production
--identValue ((CAst (Mu (ASTRING p x))):xs) = (Mu $ ASTRING x, xs)

list :: Production
list ((CToken _ RBRACK):(CToken p LBRACK):xs) = (Mu $ ALIST p [], xs)
list ((CToken _ RBRACK):(CAst (Mu (ALIST p values))):xs) = 
  (Mu $ ALIST p $ reverse values, xs)
list (x@(CToken _ RBRACK):_:xs) = list (x:xs)

listHead :: Production
listHead ((CAst value):(CToken p LBRACK):xs) = (Mu $ ALIST p [value], xs)
listHead ((CAst value):(CToken _ COMMA):(CAst (Mu (ALIST p values))):xs) =
  (Mu $ ALIST p (value:values), xs)
listHead (x@(CAst value):y@(CToken _ COMMA):_:xs) = listHead (x:y:xs) 
listHead (x@(CAst value):_:xs) = listHead (x:xs)

subst :: Production
subst ((CToken p (SUBST x)):xs) = (Mu $ ASUBST p x, xs)

merge :: Production
merge ((CAst y):(CToken _ SPACE):(CAst x):xs) = (Mu $ AMERGE x y, xs)
merge (_:xs) = merge xs

object :: Production
object ((CToken _ RBRACE):(CToken p LBRACE):xs) = (Mu $ AOBJECT p [], xs)
object ((CToken _ RBRACE):(CAst (Mu (ALIST _ ps))):(CToken p LBRACE):xs) =
  (Mu $ AOBJECT p $ reverse ps, xs)
object (x@(CToken _ RBRACE):y@(CAst (Mu (ALIST _ _))):_:xs) = object (x:y:xs)
object (x@(CToken _ RBRACE):_:xs) = object (x:xs)

property :: Production
property (x@(CAst value):y@(CAst ident):(CToken _ SPACE):xs) = 
  property (x:y:xs)
property ((CAst value):(CAst ident):xs) = (Mu $ APROP ident value, xs)
property (x@(CAst value):_:xs) = property (x:xs)
property (_:xs) = property xs -- trailling space, ex: id: value_[end]

propertiesHead :: Production
propertiesHead ((CAst prop@(Mu (APROP i  _))):xs) = 
  (Mu $ ALIST (cata go i) [prop], xs)
    where
      go (ASTRING p _) = p
      go (AMERGE p _)  = p

properties :: Production
properties ((CAst prop):(CAst (Mu (ALIST p props))):xs) = 
  (Mu $ ALIST p (prop:props), xs)
properties (x@(CAst _):(CToken _ _):xs) = properties (x:xs)
properties ((CToken _ NEWLINE):(CAst (Mu (ALIST p props))):xs) =
  (Mu $ ALIST p props, xs)
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
toCell (Elm l c s) = CToken (l, c) s
toCell EOF         = CEOF

makeParser :: Monad m => Free LALR () -> Sink Token m (Either String (Mu AST))
makeParser instr = (cataFree pure impure instr) ([], Nothing)
  where
    pure _ ((CAst (Mu (ALIST p xs))):_,_) = 
      return (Right $ Mu $ ALIST p (reverse xs))
    
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
    go (CToken _ sym) = show sym
    go CEOF             = "$"
    go (CAst mu)  = cata toString mu
    
    toString (APROP i v)     = "prop(" ++ i ++ ":" ++ v ++ ")"
    toString (AMERGE x y)    = x ++ "." ++ y 
    toString (ASTRING _ x)   = "string(" ++ x ++ ")"
    toString (ALIST _ _)     =  "list"

unexpected :: Token -> String
unexpected (Elm l c sym) = 
  "Unexpected token " ++ show sym ++ " at (" ++ show l ++ ", " ++ show c ++ ")" 

printer :: Either String (Mu AST) -> IO ()
printer = print . either id (cata go) 
  where
    go (ASTRING _ x)   = x
    go (ASUBST _ x)    = "${" ++ x ++ "}"
    go (AMERGE x y)  = "merge(" ++ x ++ " =:= " ++ y ++ ")"
    go (APROP i v)   = i ++ ": " ++ v
    go (AOBJECT _ xs)
      | null xs      = "object{}"
      | otherwise    = "object{" ++ foldr1 (\x y -> x ++ "," ++ y) xs ++ "}"
    go (ALIST _ xs)
      | null xs      = "list([])"
      | otherwise    = "list([" ++ foldr1 (\x y -> x ++ "," ++ y) xs ++ "])"

parser :: Monad m => Sink Token m (Either String (Mu AST))
parser = makeParser parseProperties
