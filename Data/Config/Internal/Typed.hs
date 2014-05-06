{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Typed
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Typed where

--------------------------------------------------------------------------------
import Data.Array
import Data.Text (Text, unpack)

--------------------------------------------------------------------------------
import Data.Config.Internal.Scoped

--------------------------------------------------------------------------------
type Var = Int

--------------------------------------------------------------------------------
data Type
    = TyVarTy Var
    | ListTy
    | AppTy Type Type
    | ForAllTy Var Type
    | RefTy Text
    | LitTy TypeLit
    deriving Eq

--------------------------------------------------------------------------------
data TypeLit
    = StringLit
    | ObjectLit
    deriving Eq

--------------------------------------------------------------------------------
data Typed
    = Typed
    { typedType  :: Type
    , typedScope :: Scoped
    }

--------------------------------------------------------------------------------
instance Show Type where
    show = showType

--------------------------------------------------------------------------------
letters :: Array Int String
letters
    = array (1, 26) (fmap (\(i, c) -> (i, [c])) $ zip [1..] ['a'..'z'])

--------------------------------------------------------------------------------
showType :: Type -> String
showType (TyVarTy v)
    = letters ! v
showType ListTy
    = "List"
showType (AppTy h l)
    = showType h ++ "[" ++ showType l ++ "]"
showType (ForAllTy v t)
    = "forall " ++ (letters ! v) ++ ". " ++ showType t
showType (RefTy t)
    = "${" ++ unpack t ++ "} type"
showType (LitTy l)
    = case l of
    StringLit -> "String"
    ObjectLit -> "Object"

--------------------------------------------------------------------------------
stringType :: Type
stringType = LitTy StringLit

--------------------------------------------------------------------------------
objectType :: Type
objectType = LitTy ObjectLit

--------------------------------------------------------------------------------
someListType :: Type
someListType = ForAllTy 1 (AppTy ListTy (TyVarTy 1))
