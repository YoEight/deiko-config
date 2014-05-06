{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Pos
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Pos where

--------------------------------------------------------------------------------
data Pos
    = Line { lineStart    :: Int
           , lineColStart :: Int
           , lineColEnd   :: Int
           }
    | Multi { multiLineStart :: Int
            , multiLineEnd   :: Int
            , multiColStart  :: Int
            , multiColEnd    :: Int
            }

--------------------------------------------------------------------------------
instance Show Pos where
    show = showPos

--------------------------------------------------------------------------------
startLine, startCol, endLine, endCol :: Pos -> Int
startLine (Line l _ _)    = l
startLine (Multi l _ _ _) = l

startCol (Line _ c _)    = c
startCol (Multi _ c _ _) = c

endLine (Line l _ _)    = l
endLine (Multi _ l _ _) = l

endCol (Line _ _ c)    = c
endCol (Multi _ _ _ c) = c

--------------------------------------------------------------------------------
showPos :: Pos -> String
showPos (Line s c n)
    = show s ++ ":" ++ show c ++ "-" ++ show n ++ ":"
showPos (Multi ls le cs ce)
    = show ls ++ "-" ++ show le ++ ":" ++ show cs ++ "-" ++ show ce ++ ":"
