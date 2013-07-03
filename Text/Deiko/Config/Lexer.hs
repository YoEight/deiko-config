module Text.Deiko.Config.Lexer where

import Control.Monad.Trans
import Data.Conduit
import Data.Foldable (Foldable, traverse_)
import Data.Char

data SubstState = None
                | Simple
                | Raw

lexer :: Monad m => Conduit Char m Token
lexer = recv (step 1 1) nop 

make :: Monad m 
     => Int
     -> Int
     -> Sym
     -> Conduit a m Token
make l c s = yield (Elm l c s)

recv :: Monad m
     => (Char -> Conduit Char m Token)
     -> Conduit Char m Token
     -> Conduit Char m Token
recv k eof = do
  input <- await
  maybe (eof >> yield EOF) k input

step :: Monad m
     => Int -- Line
     -> Int -- column
     -> Char
     -> Conduit Char m Token
step l c ' '  = make l c SPACE    >> stripSpaces step nop l (c+1)
step l c '\n' = make l c NEWLINE  >> stripNewlines step nop l
step l c '.'  = make l c DOT      >> recv (step l (c+1)) nop
step l c '='  = make l c EQUAL    >> recv (step l (c+1)) nop
step l c ':'  = make l c EQUAL    >> recv (step l (c+1)) nop
step l c '['  = make l c LBRACK   >> recv (step l (c+1)) nop
step l c ']'  = make l c RBRACK   >> recv (step l (c+1)) nop
step l c '{'  = make l c LBRACE   >> recv (step l (c+1)) nop
step l c '}'  = make l c RBRACE   >> recv (step l (c+1)) nop
step l c ','  = make l c COMMA    >> recv (step l (c+1)) nop
step l c '#'  = stripComment l
step l c '"'  = stringStep l c
step l c '$'  = substStep l c
step l c x 
  | isLetter x = makeId [] l c x
  | otherwise  = makeStr [] l c x

stringStep :: Monad m
           => Int
           -> Int
           -> Conduit Char m Token
stringStep l c = recv step1 (untermStr l c)
  where
    step1 '"' = recv step2 (make l (c+2) $ STRING "")
    step1 x   = makeStr [] l (c+1) x
                
    step2 '"' = recv (makeRawStr [] l (c+3)) (untermStr l (c+3))
    step2 x   = (make l (c+2) $ STRING "") >> step l (c+2) x

substStep :: Monad m
          => Int
          -> Int
          -> Conduit Char m Token
substStep l c = recv step1 (make l (c+1) $ STRING "$")
  where
    step1 '{' = recv (makeSubst None [] l (c+2)) (make l (c+2) $ STRING "${")
    step1  x  = makeStr "$" l (c+1) x

nop :: Monad m => Conduit i m o
nop = return ()

makeId :: Monad m 
       => String 
       -> Int 
       -> Int 
       -> Char 
       -> Conduit Char m Token
makeId acc l c x
  | validIdChar x         = recv (makeId (x:acc) l c) (produce True)
  | x == ' ' || x == '\n' = produce False >> step l (c + (length acc)) x
  | otherwise             = makeStr acc l c x
  where
    produce eof = 
      let xs = if eof then x:acc else acc in 
      yield (Elm l c (ID (reverse xs)))

makeStr :: Monad m
         => String
         -> Int
         -> Int
         -> Char
         -> Conduit Char m Token
makeStr acc l c x
  | x == '\n' = err
  | x == '$'  = recv decide (produce x)
  | x == '"'  = produce0 >> recv (step l (c+(length (x:acc)))) nop
  | otherwise = recv (makeStr (x:acc) l c) (produce x)
  where 
    err = make l c $ ERROR "Newline is not allowed in a single line String"
          
    produce0 = make l c $ STRING (reverse acc)

    produce x = make l c $ STRING (reverse (x:acc))

    produce2 x y = make l c $ STRING (reverse (y:x:acc))
                
    decide y 
      | y == '{'  =
        let c1 = c + (length acc) in
        case acc of
          [] -> recv (makeSubst Simple [] l c) (produce2 x y)
          _  -> produce0 >> recv (makeSubst Simple [] l c1) (untermStr l c1)
      | otherwise = makeStr (x:acc) l c y 

makeRawStr :: Monad m
           => String
           -> Int
           -> Int
           -> Char
           -> Conduit Char m Token
makeRawStr acc l c x
  | x == '"'  = recv step1 err
  | x == '\n' = stripNewlines rubbish err l
  | otherwise = recv (makeRawStr (x:acc) l (c+1)) err
  where
    step1 '"' = recv step2 err
    step1 x   = makeRawStr ('"':acc) l (c+1) x

    step2 '"' = produce >> recv (step l (c+3)) nop
    step2 x   = makeRawStr ('"':'"':acc) l (c+2) x

    err     = untermStr l c
    go      = produce >> recv (step l (c+3)) nop
    produce = make l c $ STRING (reverse acc)

    rubbish l c ' ' = stripSpaces (makeRawStr (' ':acc)) produce l (c+1)
    rubbish l c x   = makeRawStr (' ':acc) l c x

makeSubst :: Monad m 
          => SubstState
          -> String
          -> Int
          -> Int
          -> Char
          -> Conduit Char m Token
makeSubst state acc l c x =
  case acc of
    [] | isLetter x || 
         x == '?'      -> recv (makeSubst state [x] l c) (string [x])
       | otherwise     -> recv (makeStr (x:"{$") l c) (string [x])

    ['?'] | isLetter x -> recv (makeSubst state (x:acc) l c) (string (x:acc))
          | otherwise  -> recv (makeStr (x:"?{$") l c) (string (x:acc))

    _  | validIdChar x || 
         x == '.'        -> recv (makeSubst state (x:acc) l c) (string (x:acc))
       | x == '}'        -> make l (c-2) (SUBST $ reverse acc) >> decision
       | otherwise       -> string (x:acc)
  where
    decision = 
      let c1 = c+(length acc)+1 in
      case state of
        None   -> recv (step l c1) nop
        Simple -> recv (makeStr [] l c1) (untermStr l c1)
        Raw    -> recv (makeRawStr [] l c1) (untermStr l c1)

    string xs = make l c (STRING ("${" ++ (reverse xs)))

stripComment :: Monad m => Int -> Conduit Char m Token
stripComment l = recv go nop
  where
    go '\n' = stripNewlines step nop (l+1)
    go _    = stripComment l

stripNewlines :: Monad m 
              => (Int -> Int -> Char -> Conduit Char m Token) -- continuation
              -> Conduit Char m Token                         -- fallback
              -> Int 
              -> Conduit Char m Token
stripNewlines k f l = recv go f
  where
    go '\n' = stripNewlines k f (l+1)
    go x    = k l 0 x

stripSpaces :: Monad m 
            => (Int -> Int -> Char -> Conduit Char m Token) -- continuation
            -> Conduit Char m Token                                 -- fallback
            -> Int 
            -> Int 
            -> Conduit Char m Token
stripSpaces k f l c = recv go f
  where
    go '_' = stripSpaces k f l (c+1)
    go x   = k l c x

validIdChar :: Char -> Bool
validIdChar x = isLetter x || isDigit x || x == '-' || x == '_'

untermStr :: Monad m => Int -> Int -> Conduit Char m Token
untermStr l c = make l c $ ERROR "Unterminated String literal"

data Token = Elm Int Int Sym
           | EOF deriving Show

data Sym = ID String
         | STRING String
         | SUBST String
         | ERROR String
         | LBRACE
         | RBRACE
         | LBRACK
         | RBRACK
         | EQUAL
         | SPACE
         | NEWLINE
         | COMMA
         | DOT deriving Show

printer :: Show a => Sink a IO ()
printer = awaitForever (liftIO . print)

source :: (Monad m, Foldable f) => f a -> Source m a
source = traverse_ yield
