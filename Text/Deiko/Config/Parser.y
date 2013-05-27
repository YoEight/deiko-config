{
module Text.Deiko.Config.Parser where

import Text.Deiko.Config.Lexer

import Prelude hiding (EQ)
}

%name parser
%tokentype { Token }
%monad { Alex } { thenAlex } { returnAlex }
%lexer { lexer } { EOF }
%error { parseError }

%token
  propid        { ID $$ }
  string        { STRING $$ }
  end           { NEWLINE }
  '{'           { LBRACE }
  '}'           { RBRACE }
  '['           { LBRACK }
  ']'           { RBRACK }
  ':'           { COLON }
  ','           { COMMA }
  '.'           { DOT }
  '='           { EQ }
  '?'           { INTER }
  '_'           { SPACE }
  '$'           { DOLLAR }    
       
%%

Base : PropList                           { Root $1 }
     | PropList end                       { Root $1 }

PropList :                                { [] }
         | Prop                           { [$1] }
         | PropList end Prop              { $1 ++ [$3] }
         | PropList ',' Prop              { $1 ++ [$3] }              

Prop : propid PropTail2                   { Prop $1 $2 } 

PropTail2 : '=' PropTail4                 { mkValue $2 }       
          | '=' ObjTail                   { mkValue $2 }    
          | ObjTail                       { mkValue $1 }    

PropTail4 : Value                       { [$1] }
          | PropTail4 '_'               { $1 }
          | PropTail4 '_' Value         { $1 ++ [$3] }

Obj : '{' '_' ObjPropList               { $3 }
    | '{' ObjPropList                   { $2 }

ObjPropList : PropList ObjEnd         { POBJECT (Object $1) }

ObjEnd : end '}'                    { [] }
       | '}'                        { [] }

ObjTail :  Obj                       { [$1] }
        | ObjTail2 '_' Obj           { $1 ++ [$3] }

ObjTail2 :                           { [] }
         | ObjTail                   { $1 }

Value : string                              { PSTRING $1 }
      | propid                              { PSTRING $1 }

{
data Root = Root [Prop] deriving Show

data Prop = Prop { propName    :: String
                 , propValue   :: PropValue } deriving Show

data PropValue = PSTRING String
               | PLIST [PropValue]
               | POBJECT Object
               | PCONCAT [PropValue] deriving Show

data Object = Object [Prop] deriving Show

mkValue [x] = x
mkValue (x:xs) = PCONCAT (x:xs)

thenAlex :: Alex a -> (a -> Alex b) -> Alex b
thenAlex = (>>=)

returnAlex :: a -> Alex a
returnAlex = return

parseError t = do
  (_, l, c) <- getPosn
  alexError ("l" ++ (show l) ++ ", c" ++ (show c) ++ " on token " ++ (show t))
}
