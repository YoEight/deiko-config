-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Config.Internal.SrcLoc
-- Copyright   :  (C) 2013 Yorick Laupa
-- License     :  (see the file LICENSE)
--
-- Maintainer  :  Yorick Laupa <yo.eight@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Data.Config.Internal.SrcLoc where

data GenLocated l e = L l e deriving (Eq, Ord)
type Located e  = GenLocated SrcSpan e

data SrcLoc = SrcLoc !String

data SrcSpan
    = SrcSpanOneLine
      { ssSrcName :: !String
      , ssLine    :: !Int
      , ssSCol    :: !Int
      , ssECol    :: !Int
      }
    | SrcSpanMultiLine
      { ssSrcName :: !String
      , ssSLine   :: !Int
      , ssELine   :: !Int
      , ssSCol    :: !Int
      , ssECol    :: !Int
      }
    | SrcSpanPoint
      { ssSrcName :: !String
      , ssLine    :: !Int
      , ssCol     :: !Int
      }

mkPointSpan :: SrcSpan -> SrcSpan -> SrcSpan
mkPointSpan (SrcSpanPoint n sl sc) (SrcSpanPoint _ el ec)
    | sl == el  = SrcSpanOneLine n sl sc ec
    | otherwise = SrcSpanMultiLine n sl el sc ec

instance (Show l, Show e) => Show (GenLocated l e) where
    show (L l e) = show l ++ " " ++ show e

instance Show SrcSpan where
    show (SrcSpanOneLine _ l s e) =
        "(" ++ show l ++ ", " ++ show s ++ "-" ++ show e ++ ")"
    show (SrcSpanMultiLine _ ls le s e) =
        "(" ++ show ls ++ "-" ++ show le ++ ", " ++ show s ++ "-" ++
        show e ++ ")"
    show (SrcSpanPoint _ l c) =
        "(" ++ show l ++ "," ++ show c ++ ")"
