{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.AST
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.AST where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
data Expr p t
    = ID Text
    | STRING Text
    | LIST [p t]
    | SUBST Text
    | MERGE (p t) (p t)
    | OBJECT [Prop p t] deriving Show

--------------------------------------------------------------------------------
data AST t
    = AST
      { astExpr :: Expr AST t
      , astTag  :: t
      } deriving Show

--------------------------------------------------------------------------------
data Prop p t
    = Prop
      { propName :: !Text
      , propAST  :: p t
      } deriving Show
