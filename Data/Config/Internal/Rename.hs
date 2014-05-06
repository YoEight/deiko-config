{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Rename
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Rename (rename) where

--------------------------------------------------------------------------------
import Prelude hiding (null)

--------------------------------------------------------------------------------
import Data.Text (Text, append, null)

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Pos
import Data.Config.Internal.Scoped

--------------------------------------------------------------------------------
infixr 7 <.>

--------------------------------------------------------------------------------
(<.>) :: Text -> Text -> Text
(<.>) a b
    | null a    = b
    | otherwise = a `append` ("." `append` b)

--------------------------------------------------------------------------------
rename :: [Prop AST Pos] -> [Prop AST Scoped]
rename = fmap renameProp

--------------------------------------------------------------------------------
renameProp :: Prop AST Pos -> Prop AST Scoped
renameProp (Prop n v) = Prop n (renameAST n n v)

--------------------------------------------------------------------------------
renameAST :: Text -> Text -> AST Pos -> AST Scoped
renameAST prop scope (AST expr pos) =
    case expr of
        ID i      -> AST (ID i) $ Scoped prop scope pos
        STRING i  -> AST (STRING i) $ Scoped prop scope pos
        LIST xs   -> renameList prop scope xs pos
        SUBST i   -> AST (SUBST i) $ Scoped prop scope pos
        MERGE l r -> renameMerge prop scope l r pos
        OBJECT ps -> renameObject prop scope ps pos

--------------------------------------------------------------------------------
renameList :: Text -> Text -> [AST Pos] -> Pos -> AST Scoped
renameList prop scope xs pos
    = let xs' = fmap (renameAST prop "") xs in
      AST (LIST xs') $ Scoped prop scope pos

--------------------------------------------------------------------------------
renameMerge :: Text -> Text -> AST Pos -> AST Pos -> Pos -> AST Scoped
renameMerge prop scope l r pos
    = let sl = renameAST prop scope l
          sr = renameAST prop scope r in
      AST (MERGE sl sr) $ Scoped prop scope pos

--------------------------------------------------------------------------------
renameObject :: Text -> Text -> [Prop AST Pos] -> Pos -> AST Scoped
renameObject prop scope ps pos
    = let go (Prop n v)
              = let scope' = scope <.> n
                    v'     = renameAST prop scope' v in
                Prop scope' v'
          ps' = fmap go ps in
      AST (OBJECT ps') $ Scoped prop scope pos
