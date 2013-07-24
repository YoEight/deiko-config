module Text.Deiko.Config.Lexer (lexer, Token(..), Sym(..)) where

import Control.Arrow ((&&&))
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT, evalStateT, modify, gets)
import Data.Conduit (Conduit, ConduitM, yield, await)
import Data.ByteString.Char8 (unpack)
import Data.Foldable (Foldable, traverse_)
import Data.Char (isLetter, isDigit)
import Text.Deiko.Config.Internal hiding (makeId)

data StringState = None
                | Simple
                | Raw [String]

data LexerState = LexerState { lexLine  :: Int 
                             , lexCol   :: Int 
                             , lexBrace :: Int
                             , lexBrack :: Int }

type Lexer m a = StateT LexerState (ConduitM Char Token m) a

lexer :: Monad m => Conduit Char m Token
lexer = evalStateT (recv step nop) start
  where
    start = LexerState 1 1 0 0

make :: Monad m => Sym -> Lexer m ()
make s = do
  (l, c) <- gets (lexLine &&& lexCol)
  lift $ yield (Elm l c s)

recv :: Monad m => (Char -> Lexer m ()) -> Lexer m () -> Lexer m ()
recv k eof = do
  input <- lift await
  maybe (eof >> end) k input
  where
    end = do
      (br, bk) <- gets (lexBrace &&& lexBrack)
      when (br /= 0) unmatchedBrace
      when (bk /= 0) unmatchedBracket
      lift $ yield EOF

step :: Monad m => Char -> Lexer m ()
step ' '  = make SPACE    >> incrCol  >> stripSpaces step nop
step '\n' = make NEWLINE  >> incrLine >> setCol 1 >> stripNewlines step nop
step '.'  = make DOT      >> incrCol  >> recv step nop
step '='  = make EQUAL    >> incrCol  >> recv step nop
step ':'  = make EQUAL    >> incrCol  >> recv step nop
step ','  = make COMMA    >> incrCol  >> recv step nop
step '#'  = stripComment
step '"'  = stringStep
step '$'  = substStep
step '{'  = make LBRACE   >> incrCol  >> incrBrace >> recv step nop
step '}'  = do
  i <- gets lexBrace
  if i <= 0 then unmatchedBrace
  else make RBRACE >> incrCol >> decrBrace >> recv step nop
step '['  = make LBRACK   >> incrCol  >> incrBracket >> recv step nop
step ']'  = do
  i <- gets lexBrack
  if i <= 0 then unmatchedBracket
  else make RBRACK >> incrCol  >> decrBracket >> recv step nop
step x 
  | isLetter x = makeId [] x
  | otherwise  = makeStr None [] x

stringStep :: Monad m => Lexer m ()
stringStep = recv step1 untermStr
  where
    step1 '"' = recv step2 (make $ STRING "")
    step1 x   = makeStr Simple [] x
                
    step2 '"' = recv (makeStr (Raw []) []) untermStr
    step2 x   = (make $ STRING "") >>
                incrColBy 2        >>
                step x

substStep :: Monad m => Lexer m ()
substStep = recv step1 (incrCol >> (make $ STRING "$"))
  where
    step1 '{' = incrColBy 2 >> recv (makeSubst None []) (make $ STRING "${")
    step1  x  = incrCol >> makeStr None "$" x

nop :: Monad m => Lexer m ()
nop = return ()

makeId :: Monad m => String -> Char -> Lexer m ()
makeId acc x
  | validIdChar x           = recv (makeId (x:acc)) (produce True)
  | x == ' ' || x == '\n' ||
    x == '.' || x == ':'  ||   
    x == '=' || x == '{'  ||
    x == '}'                = produce False >> incrColBy (length acc) >> step x
  | otherwise               = makeStr None acc x
  where
    produce eof = 
      let xs = if eof then x:acc else acc in
      make $ ID $ reverse xs

makeStr :: Monad m => StringState -> String -> Char -> Lexer m ()
makeStr state acc x
  | x == '\n' = case state of 
                  None   -> produce0 >> incrColBy (length acc) >> step x
                  Simple -> err
                  Raw xs -> recv (makeStr (Raw (acc:xs)) []) untermStr
  | x == '$'  = case state of
                  None -> recv decide (produce x)
                  _    -> recv decide untermStr
  | x == '"'  = case state of
                  None   -> recv (makeStr state ('"':acc)) (produce x)
                  Simple -> produce0                   >> 
                            incrColBy ((length acc)+2) >> 
                            recv step nop
                  Raw _  -> recv step1 untermStr
  | x == ',' || 
    x == ']' = case state of
                 None -> produce0 >> incrColBy (length acc) >> step x
                 _    -> recv (makeStr state (x:acc)) untermStr
  | otherwise = case state of
                  None -> recv (makeStr state (x:acc)) (produce x)
                  _    -> recv (makeStr state (x:acc)) untermStr
  where 
    step1 '"' = recv step2 untermStr
    step1 x   = makeStr state ('"':acc) x

    step2 '"' =  produceRaw >>
                 finalLine  >>
                 finalCol   >>
                 recv step nop
    step2 x   = makeStr state ('"':'"':acc) x
    
    err = make $ ERROR "Newline is not allowed in a single line String"

    finalLine = 
      case state of
        Raw xs -> incrLineBy $ length xs

    finalCol =
      case state of
        Raw []     -> incrColBy (length acc + 3)
        Raw (xs:_) -> setCol (1 + length acc + 3)

    produceRaw = 
      case state of
        Raw xs -> make $ STRING str
          where
            str = foldl go "" (reverse $ fmap (reverse . trim) xs)
            go [] b = b
            go xs b = xs ++ " " ++ b
          
    produce0     = make $ STRING (reverse acc)
    produce x    = make $ STRING (reverse (x:acc))
    produce2 x y = make $ STRING (reverse (y:x:acc))

    decide y 
      | y == '{'  =
        let c1 = length acc in
        case acc of
          [] -> recv (makeSubst state []) (produce2 x y)
          _  -> produce0 >> incrColBy c1 >> recv (makeSubst state []) untermStr
      | otherwise = makeStr state (x:acc) y 

makeSubst :: Monad m  => StringState -> String -> Char -> Lexer m ()
makeSubst state acc x =
  case acc of
    [] | isLetter x || 
         x == '?'      -> recv (makeSubst state [x]) (string [x])
       | otherwise     -> recv (makeStr state (x:"{$")) (string [x])

    ['?'] | isLetter x -> recv (makeSubst state (x:acc)) (string (x:acc))
          | otherwise  -> recv (makeStr state (x:"?{$")) (string (x:acc))

    _  | validIdChar x || 
         x == '.'        -> recv (makeSubst state (x:acc)) (string (x:acc))
       | x == '}'        -> produce >> decision
       | otherwise       -> string (x:acc)
  where
    decision =
      do incrColBy ((length acc)+1)
         case state of
           None -> recv step nop
           _    -> recv (makeStr state []) untermStr

    string xs = make (STRING ("${" ++ (reverse xs)))

    produce = do
      (l, c) <- gets (lexLine &&& lexCol)
      lift $ yield (Elm l (c-2) (SUBST $ reverse acc))

stripComment :: Monad m => Lexer m ()
stripComment = recv go nop
  where
    go '\n' = incrLine >> stripNewlines step nop
    go _    = stripComment

stripNewlines :: Monad m 
              => (Char -> Lexer m ()) -- continuation
              -> Lexer m ()           -- fallback
              -> Lexer m ()
stripNewlines k f = recv go f
  where
    go '\n' = incrLine >> stripNewlines k f
    go x    = setCol 1 >> k x

stripSpaces :: Monad m 
            => (Char -> Lexer m ()) -- continuation
            -> Lexer m ()           -- fallback
            -> Lexer m ()
stripSpaces k f = recv go f
  where
    go ' ' = incrCol >> stripSpaces k f
    go x   = k x

incrLineBy :: Monad m => Int -> StateT LexerState m ()
incrLineBy n = modify $ \s@LexerState{lexLine=i} -> s{lexLine=i + n}

incrLine :: Monad m => StateT LexerState m ()
incrLine = incrLineBy 1

onCol :: Monad m => (Int -> Int) -> StateT LexerState m ()
onCol f = modify $ \s@LexerState{lexCol=i} -> s{lexCol=f i}

incrColBy :: Monad m => Int -> StateT LexerState m ()
incrColBy = onCol . (+)

incrCol :: Monad m => StateT LexerState m ()
incrCol = incrColBy 1

setCol :: Monad m => Int -> StateT LexerState m ()
setCol = onCol . const

onBrace :: Monad m => (Int -> Int) -> StateT LexerState m ()
onBrace f = modify $ \s@LexerState{lexBrace=i} -> s{lexBrace=f i}

incrBrace :: Monad m => StateT LexerState m ()
incrBrace = onBrace (+1)

decrBrace :: Monad m => StateT LexerState m ()
decrBrace = onBrace (\x -> x - 1)

onBracket :: Monad m => (Int -> Int) -> StateT LexerState m ()
onBracket k = modify $ \s@LexerState{lexBrack=i} -> s{lexBrack=k i}

incrBracket :: Monad m => StateT LexerState m ()
incrBracket = onBracket (+1)

decrBracket :: Monad m => StateT LexerState m ()
decrBracket = onBracket (\x -> x - 1)

trim :: String -> String
trim xs =
  let (_, xs1) = span (== ' ') xs
      (_, xs2) = span (== ' ') (reverse xs1) in
  reverse xs2

validIdChar :: Char -> Bool
validIdChar x = isLetter x || isDigit x || x == '-' || x == '_'

untermStr :: Monad m => Lexer m ()
untermStr = make $ ERROR "Unterminated String literal"

unmatchedBrace :: Monad m => Lexer m ()
unmatchedBrace = make $ ERROR "Unmatched brace"

unmatchedBracket :: Monad m => Lexer m ()
unmatchedBracket = make $ ERROR "Unmatched bracket"
