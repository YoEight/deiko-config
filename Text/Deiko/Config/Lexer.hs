module Text.Deiko.Config.Lexer where

import Data.Conduit
import Data.Char

lexer :: Monad m => Conduit Char m Token
lexer = recv (step 0 0) nop 

make :: Monad m 
     => Integer
     -> Integer
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
     => Integer -- Line
     -> Integer -- column
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
           => Integer
           -> Integer
           -> Conduit Char m Token
stringStep l c = recv step1 (untermStr l c)
  where
    step1 '"' = recv step2 (make l (c+2) $ STRING "")
    step1 x   = makeStr [] l (c+1) x
                
    step2 '"' = recv (makeRawStr [] l (c+3)) (untermStr l (c+3))
    step2 x   = (make l (c+2) $ STRING "") >> step l (c+2) x

substStep :: Monad m
          => Integer
          -> Integer
          -> Conduit Char m Token
substStep l c = recv step1 (make l (c+1) $ STRING "$")
  where
    step1 '{' = recv (makeSubst [] l (c+2)) (make l (c+2) $ STRING "${")
    step1  x  = makeStr "$" l (c+1) x

nop :: Monad m => Conduit i m o
nop = return ()

makeId :: Monad m 
       => String 
       -> Integer 
       -> Integer 
       -> Char 
       -> Conduit Char m Token
makeId acc l c x
  | validIdChar x         = recv (makeId (x:acc) l (c+1)) produce
  | x == ' ' || x == '\n' = produce >> step l c x
  | otherwise             = makeStr acc l c x
  where
    produce = yield (Elm l c (ID (reverse acc)))

makeStr :: Monad m
         => String
         -> Integer
         -> Integer
         -> Char
         -> Conduit Char m Token
makeStr acc l c x
  | x == '\n'             = err
  | x == '$'              = recv decide (produce x)
  | x == '"'              = produce x >> recv (step l (c+1)) nop
  | otherwise             = recv (makeStr (x:acc) l (c+1)) (produce x)
  where 
    err = make l c $ ERROR "Newline is not allowed in a single line String"

    produce x = yield (Elm l c (STRING (reverse (x:acc))))

    produce2 x y = yield (Elm l c (STRING (reverse (y:x:acc))))
                
    decide y 
      | y == '{'  = recv (makeSubst [] l (c+2)) (produce2 x y)
      | otherwise = makeStr (x:acc) l (c+1) y 

makeRawStr :: Monad m
           => String
           -> Integer
           -> Integer
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
          => String
          -> Integer
          -> Integer
          -> Char
          -> Conduit Char m Token
makeSubst acc l c x =
  case acc of
    [] | isLetter x || 
         x == '?'      -> recv (makeSubst [x] l (c+1)) (string [x])
       | otherwise     -> recv (makeStr (x:"{$") l (c+1)) (string [x])

    ['?'] | isLetter x -> recv (makeSubst (x:acc) l (c+1)) (string (x:acc))
          | otherwise  -> recv (makeStr (x:"?{$") l (c+1)) (string (x:acc))

    _  | validIdChar x || 
         x == '.'         -> recv (makeSubst (x:acc) l (c+1)) (string (x:acc))
       | x == '}'         -> make l c (SUBST $ reverse (x:acc)) >> 
                             recv (step l (c+1)) nop
       | otherwise        -> string (x:acc)
  where
    string xs = make l c (STRING ("${" ++ (reverse xs)))

stripComment :: Monad m => Integer -> Conduit Char m Token
stripComment l = recv go nop
  where
    go '\n' = stripNewlines step nop (l+1)
    go _    = stripComment l

stripNewlines :: Monad m 
              => (Integer -> Integer -> Char -> Conduit Char m Token) -- continuation
              -> Conduit Char m Token                                 -- fallback
              -> Integer 
              -> Conduit Char m Token
stripNewlines k f l = recv go f
  where
    go '\n' = stripNewlines k f (l+1)
    go x    = k l 0 x

stripSpaces :: Monad m 
            => (Integer -> Integer -> Char -> Conduit Char m Token) -- continuation
            -> Conduit Char m Token                                 -- fallback
            -> Integer 
            -> Integer 
            -> Conduit Char m Token
stripSpaces k f l c = recv go f
  where
    go '_' = stripSpaces k f l (c+1)
    go x   = k l c x

validIdChar :: Char -> Bool
validIdChar x = isLetter x || isDigit x || x == '-' || x == '_'

untermStr :: Monad m => Integer -> Integer -> Conduit Char m Token
untermStr l c = make l c $ ERROR "Unterminated String literal"

data Token = Elm Integer Integer Sym
           | EOF

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
         | DOT
