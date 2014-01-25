-----------------------------------------------------------------------------
-- |
-- Module      :  Deiko.Config.Internal.DkToken
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Config.Internal.Token where

import qualified Data.Text as T

import Data.Config.Internal.SrcLoc

type Token = Located IToken

data IToken = ITinclude
            | ITobrace
            | ITcbrace
            | ITobrack
            | ITcbrack
            | ITequal
            | ITspace
            | ITnewline
            | ITcomma
            | ITdot
            | ITquestion
            | ITdollar
            | ITat
            | ITeof
            | ITvarid !T.Text
            | ITstring !T.Text deriving Show
