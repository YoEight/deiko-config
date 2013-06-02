module Text.Deiko.Config.Semantic (compile
                                  ,Root
                                  ,Prop(..)
                                  ,PropValue(..)
                                  ,Config
                                  ,configRegister
                                  ,Register
                                  ,Object(..)) where

import           Control.Monad            (when, (<=<))
import           Control.Monad.State
import           Control.Monad.Trans      (lift)

import           Data.Foldable            (traverse_)
import qualified Data.Map                 as M
import           Data.Traversable         (traverse)

import           Text.Deiko.Config.Parser (Object (..), Prop (..),
                                           PropValue (..), Root (..),
                                           parseConfig)

data Type = Defined String
          | Joker

data Config = Config { configRoot     :: Root
                     , configRegister :: Register }

type Register = M.Map String PropValue

compile :: String -> Either String Config
compile = semantic <=< parseConfig

emptyRegister :: Register
emptyRegister = M.empty

register :: PropValue -> PropValue -> StateT Register (Either String) ()
register path value =
  modify (M.insertWith const (showPath path) value)

failure :: String -> StateT Register (Either String) a
failure = lift . Left

showType :: Type -> String
showType Joker       = "*joker*"
showType (Defined x) = x

semantic :: Root -> Either String Config
semantic r@(Root props) =
  fmap mk (execStateT (traverse_ go props) emptyRegister)
  where
    mk reg = Config r reg

    go p@(Prop ident value) =
      let (x, v) = liftValue ident value in
      checkValue x v

    checkValue ident v@(PCONCAT (x:xs)) =
      let definedType = getType x in
      checkConcatType ident definedType xs >>
      register ident v
    checkValue ident v@(POBJECT (Object props)) =
      let step (Prop pid value) =
            checkValue (PIDENT ident pid) value in
      traverse_ step props >>
      register ident v
    checkValue ident v = register ident v

    checkConcatType ident defType (x:xs) =
      let typ  = getType x
          path = showPath ident in
      when (not (typecheck defType typ))
             (failure (concatErrorMsg path defType typ)) >>
      checkConcatType ident (switchType defType typ) xs
    checkConcatType _ _ [] = return ()

    typecheck Joker _                 = True
    typecheck _ Joker                 = True
    typecheck (Defined x) (Defined y) = x == y

    switchType Joker x = x
    switchType x _     = x

    getType (PSUBST _)  = Joker
    getType (PLIST _)   = Defined "list"
    getType (POBJECT _) = Defined "object"
    getType (PSTRING _) = Defined "string"

    liftValue x@(PSTRING _) v = (x, v)
    liftValue (PIDENT x y) v  = liftValue x (POBJECT (Object [Prop y v]))


showPath :: PropValue -> String
showPath (PSTRING x)  = x
showPath (PIDENT x y) = (showPath x) ++ "." ++ (showPath) y

concatErrorMsg prop expected actual =
  "Concatenation doesn't typecheck for property " ++ prop ++ "; expected: " ++
  (showType expected) ++ ", actual: " ++ (showType actual)


