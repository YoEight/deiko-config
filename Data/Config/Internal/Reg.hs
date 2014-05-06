--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Reg
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Reg where

--------------------------------------------------------------------------------
import Data.Map.Strict

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Typed

--------------------------------------------------------------------------------
data Reg
    = Reg
      { regTypes :: !(Map Text Type)
      , regAST   :: !(Map Text (AST Typed))
      }
