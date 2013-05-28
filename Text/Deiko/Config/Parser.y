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
  subst         { SUBST $$ }
  newline       { NEWLINE }
  '{'           { LBRACE }
  '}'           { RBRACE }
  '['           { LBRACK }
  ']'           { RBRACK }
  ','           { COMMA }
  '='           { EQ }
  '_'           { SPACE } 
  '.'           { DOT }  
       
%%

Base : PropList                         { Root $1 }
     | PropList newline                 { Root $1 }
     |                                  { Root [] }

PropList : Prop                         { [$1] }
         | PropList newline Prop            { $1 ++ [$3] }
         | PropList ',' '_' Prop        { $1 ++ [$4] }  
         | PropList ',' newline Prop    { $1 ++ [$4] }            
         | PropList ',' Prop            { $1 ++ [$3] }

Prop : Ident '_' PropTail               { Prop $1 $3 }
     | Ident PropTail                   { Prop $1 $2 }

Ident : propid                          { PSTRING $1 }
      | Ident '.' propid                { PIDENT $1 (PSTRING $3) }

PropTail : '=' PropTail1                { mkValue $2 }          
         | Obj                          { $1 }    

PropTail1 : Value                       { [$1] }
          | PropTail1 '_'               { $1 }
          | PropTail1 '_' Value         { $1 ++ [$3] }

Obj : '{' '_' ObjPropList                { $3 }
    | '{' newline ObjPropList            { $3 }
    | '{' ObjPropList                    { $2 }

ObjPropList : PropList ObjEnd           { POBJECT (Object $1) }

ObjEnd : '}'                            { () }
       | newline '}'                    { () }

List : '[' ListTail ']'                 { PLIST $2 }

ListTail :                              { [] }
         | Value                        { [$1] }
         | ListTail '_'                 { $1 }
         | ListTail newline             { $1 }
         | ListTail ',' '_' Value       { $1 ++ [$4] }  
         | ListTail ',' newline Value   { $1 ++ [$4] }  
         | ListTail ',' Value           { $1 ++ [$3] }

Value : string                          { PSTRING $1 }
      | propid                          { PSTRING $1 }
      | subst                           { PSUBST $1 }
      | Obj                             { $1 }
      | List                            { $1 }

{
data Root = Root [Prop] deriving Show

data Prop = Prop { propName    :: PropValue
                 , propValue   :: PropValue } deriving Show

data PropValue = PSTRING String
               | PLIST [PropValue]
               | POBJECT Object
               | PCONCAT [PropValue] 
               | PIDENT PropValue PropValue
               | PSUBST String deriving Show

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
