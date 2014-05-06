--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Scoped
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Scoped where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
import Data.Config.Internal.Pos

--------------------------------------------------------------------------------
data Scoped
    = Scoped
      { scopeProp :: !Text
      , scopeName :: !Text
      , scopePos  :: !Pos
      } deriving Show
