{
module Text.Deiko.Config.Lexer where

import Prelude hiding (EQ)
import Control.Monad (liftM, when)
}

%wrapper "monadUserState"

$char    = [a-zA-Z0-9\-\_]
$space   = $white # \n
$newline = \n
@rubbish = [$newline $space]*

tokens :-
  <0> $space*$newline+@rubbish  { token (\_ _ -> NEWLINE) } 
  <0> $space+                   { token (\_ _ -> SPACE) }
  <0> \{                        { setToken (+1) LBRACE }
  <0> \}                        { setToken (\x -> x - 1) RBRACE }
  <0> \[                        { token (\_ _ -> LBRACK) }
  <0> \]                        { token (\_ _ -> RBRACK) }
  <0> $space*[\=\:]$space*      { token (\_ _ -> EQ) }
  <0> \?                        { token (\_ _ -> INTER) }
  <0> \$\{                      { begin subst }
  <0> \#.*@rubbish              { skip }
  <0> \.                        { token (\_ _ -> DOT) }
  <0> \,                        { token (\_ _ -> COMMA) }
  <0> $char+                    { token (\(_, _, _, s) n -> ID (take n s) ) }
  <0> \"                        { startString `andBegin` string }
  <0> \"\"\"@rubbish            { startString `andBegin` raw_string }
  <raw_string> @rubbish\"\"\"   { makeString `andBegin` 0 }
  <raw_string> \"               { appendStringValueWith "\""}
  <raw_string> @rubbish         { appendStringValueWith " "}
  <raw_string> \b               { appendStringValueWith "\b" }
  <raw_string> \t               { appendStringValueWith "\t" }
  <raw_string> \f               { appendStringValueWith "\f" }
  <raw_string> (. # \")+        { appendStringValue }
  <string> (. # [\"])+          { appendStringValue }
  <string> \n                   { \_ _ -> alexError "Non-terminated string" }
  <string> \"                   { makeString `andBegin` 0 }
  <subst> $char+                { token (\(_, _, _, s) n -> SUBST (take n s)) }
  <subst> \}                    { begin 0 }
  <subst> .                     { \_ _ -> alexError "Invalid character in string substitution" }

{
data Token = ID String
           | STRING String
           | SUBST String
           | LBRACE
           | RBRACE
           | LBRACK
           | RBRACK
           | EQ
           | INTER
           | DOLLAR
           | SHARP
           | SPACE
           | NEWLINE
           | COMMA
           | DOT
           | EOF deriving Show

data AlexUserState = AlexUserState { objectDepth :: Int 
                                   , stringValue :: String }

alexInitUserState = AlexUserState 0 ""

alexEOF = return EOF

get      = Alex $ \s   -> Right (s, alex_ust s)
put u    = Alex $ \s -> Right (s{alex_ust=u}, ())
modify f = put . f =<< get

getStringValue = liftM stringValue get

getObjectDepth = liftM objectDepth get  

modifyStringValue f = modify go
  where
    go s@(AlexUserState{stringValue=v}) = s{stringValue=(f v)}

modifyObjectDepth f = modify go
  where
    go s@(AlexUserState{objectDepth=d}) = s{objectDepth=(f d)}

putStringValue v = modifyStringValue (const v)

appendStringValue r@(_,_,_,i) l = do
   modifyStringValue (++ (take l i))
   skip r l

appendStringValueWith str i l = do
   modifyStringValue (++ str)
   skip i l

appendEscapedChar r@(_,_,_,i) l = do
  let (_:c:_) = take l i
  appendStringValueWith [c] r l

makeString _ _ = do
  v     <- getStringValue
  return $ STRING v

startString i r =
  do putStringValue ""
     skip i r

setToken f token _ _ =
  do modifyObjectDepth f
     return token

getState = Alex $ \x -> Right (x, x)

getPosn = do
  AlexState (AlexPn x l c) _ _ _ _ _ <- getState
  return (x, l, c)

lexerP :: String -> Either String [Token]
lexerP input = runAlex input loop
  where
    loop = alexMonadScan >>= go
    go EOF = 
      do depth <- getObjectDepth
         code  <- alexGetStartCode
         when (code == raw_string) (alexError "Non-terminated raw string")
         when (depth /= 0) (alexError "Unclosed brace")
         return [EOF]
    go x   = liftM (x:) loop
          
lexer :: (Token -> Alex a) -> Alex a
lexer k = alexMonadScan >>= go
  where
    go EOF = 
      do depth <- getObjectDepth
         code  <- alexGetStartCode
         when (code == raw_string) (alexError "Non-terminated raw string")
         when (depth /= 0) (alexError "Unclosed brace")
         k EOF
    go x   = k x

}
