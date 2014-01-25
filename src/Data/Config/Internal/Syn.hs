-----------------------------------------------------------------------------
-- |
-- Module      :  Deiko.Config.Internal.Syn
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Config.Internal.Syn where

import qualified Data.Text as T

import Data.Config.Internal.SrcLoc

type PostTcType = Type

data Module name =
    Module
    { modImports :: [IncludeDecl]
    , modDecls   :: [Decl name]
    } deriving Show

type ModuleName = T.Text

newtype IncludeDecl = IncludeDecl (Located ModuleName) deriving Show

data Decl id = ValD (Bind id) deriving Show

type Bind id = BindLR id id
data BindLR idL idR = PropBind idL (LExpr idR) deriving Show

type LExpr id = Located (Expr id)

data Expr id = Lit (Lit id)
             | List PostTcType [LExpr id]
             | Include id
             | Merge (LExpr id) (LExpr id)
             | Object [Bind id]

instance Show id => Show (Expr id) where
    show (Lit l)     = "(Lit " ++ show l ++ ")"
    show (List _ xs) = "(List " ++ show xs ++ ")"
    show (Include i) = "(Include " ++ show i ++ ")"
    show (Merge a b) = "(Merge " ++ show a ++ " " ++ show b ++ ")"
    show (Object xs) = "(Object " ++ show xs ++ ")"

data Lit id = LString T.Text deriving Show

data Type = VarT Var
          | AppT Type Type deriving Show

data Var
    = TyVar
      {
        varName   :: !Name
      , varUnique :: !Int
      }
    | Id
      {
        varName   :: !Name
      , varUnique :: !Int
      , varType   :: Type
      , varScope  :: Scope
      } deriving Show

data Scope = Global | Local deriving Show

data Name = Name
            { nameSort   :: NameSort
            , nameOcc    :: !OccName
            , nameUnique :: !Int
            , nameLoc    :: !SrcSpan
            } deriving Show

data NameSort = External ModuleName
              | Internal
              | System deriving Show

data NameSpace = VarName deriving Show

data OccName = OccName
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !T.Text
    } deriving Show
