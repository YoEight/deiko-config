module Text.Deiko.Config.Semantic where

import Control.Monad            (when, (>=>))

import Data.Foldable            (traverse_)
import Data.Traversable         (traverse)

import Text.Deiko.Config.Parser (Object (..), Prop (..), PropValue (..),
                                 Root (..), parseConfig)

data Type = Defined String
          | Joker

showType :: Type -> String
showType Joker       = "*jocker*"
showType (Defined x) = x

checking :: Root -> Either String Root
checking (Root props) = fmap Root (traverse go props)
  where
    go p@(Prop _ value) = checkValue value >>
                          return p

    checkValue (PCONCAT (x:xs)) =
      let definedType = getType x in
      checkConcatType definedType xs
    checkValue (POBJECT (Object props)) =
      traverse_ go props
    checkValue _ = return ()

    checkConcatType defType (x:xs) =
      let typ = getType x in
      when (not (typecheck defType typ))
             (Left (concatErrorMsg defType typ)) >>
      checkConcatType (switchType defType typ) xs
    checkConcatType _ [] = return ()

    typecheck Joker _                 = True
    typecheck _ Joker                 = True
    typecheck (Defined x) (Defined y) = x == y

    switchType Joker x = x
    switchType x _     = x

    getType (PSUBST _)  = Joker
    getType (PLIST _)   = Defined "list"
    getType (POBJECT _) = Defined "object"
    getType (PSTRING _) = Defined "string"

concatErrorMsg expected actual =
  "Concatenation doesn't typecheck. expected: " ++
  (showType expected) ++ ", actual: " ++ (showType actual)


