{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Register
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Register (register, simplify) where

--------------------------------------------------------------------------------
import Data.List hiding (insert)

--------------------------------------------------------------------------------
import Data.Map.Strict
import Data.Text (Text, append)

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Reg
import Data.Config.Internal.Typed

--------------------------------------------------------------------------------
type Rg = Map Text (AST Typed)

--------------------------------------------------------------------------------
register :: Map Text Type -> [Prop AST Typed] -> Reg
register tys ps = Reg tys (registerProps empty ps) where

--------------------------------------------------------------------------------
registerProps :: Rg -> [Prop AST Typed] -> Rg
registerProps mast []
      = mast
registerProps mast ((Prop n v):xs)
      = let (mast', v') = registerAST mast v in
        registerProps (insert n v' mast') xs

--------------------------------------------------------------------------------
registerAST :: Rg -> AST Typed -> (Rg, AST Typed)
registerAST reg ast@(AST expr t)
    = case expr of
    OBJECT ps -> registerObject reg t ps
    MERGE l r -> registerAST reg $ merging t l r
    _         -> (reg, ast)

--------------------------------------------------------------------------------
registerObject :: Rg -> Typed -> [Prop AST Typed] -> (Rg, AST Typed)
registerObject reg ty ps
    = (registerProps reg ps, AST (OBJECT ps) ty)

--------------------------------------------------------------------------------
simplify :: Reg -> AST Typed -> AST Typed
simplify conf ast@(AST expr ty)
    = case expr of
    LIST xs   -> simplifyList conf ty xs
    SUBST s   -> reg ! s
    MERGE l r -> simplifyMerge conf ty l r
    OBJECT ps -> simplifyObject conf ty ps
    _         -> ast
  where
    reg = regAST conf

--------------------------------------------------------------------------------
simplifyList :: Reg -> Typed -> [AST Typed] -> AST Typed
simplifyList conf ty xs
    = AST (LIST (fmap (simplify conf) xs)) ty

--------------------------------------------------------------------------------
simplifyMerge :: Reg -> Typed -> AST Typed -> AST Typed -> AST Typed
simplifyMerge conf ty l r
    = loop l r where
      loop ll@(AST la _) rr@(AST ra _)
          = case (la, ra) of
          (SUBST s, _)           -> loop (reg ! s) rr
          (_, SUBST s)           -> loop ll (reg ! s)
          (ID il, ID ir)         -> mergeId ty il ir
          (STRING sl, STRING sr) -> mergeString ty sl sr
          (ID i, STRING s)       -> mergeString ty i s
          (STRING s, ID i)       -> mergeString ty s i
          (LIST xs, LIST vs)     -> mergeList ty xs vs
          (OBJECT p, OBJECT v)   -> mergeObject ty p v
          (MERGE ml mr, _) ->
              let ll' = simplifyMerge conf ty ml mr in
              loop ll' rr
          (_, MERGE ml mr) ->
              let rr' = simplifyMerge conf ty ml mr in
              loop ll rr'
          _ -> error absurdMsg

      reg = regAST conf
      absurdMsg
          = "impossible situation. Data.Config.Internal.Register.simplifyMerge"

--------------------------------------------------------------------------------
simplifyObject :: Reg -> Typed -> [Prop AST Typed] -> AST Typed
simplifyObject conf ty xs
    = let xs' = fmap (\(Prop n v) -> Prop n (simplify conf v)) xs in
      AST (OBJECT xs') ty

--------------------------------------------------------------------------------
merging :: Typed -> AST Typed -> AST Typed -> AST Typed
merging ty l r
    = loop l r where
      loop ll@(AST la _) rr@(AST ra _)
          = case (la, ra) of
          (ID il, ID ir)         -> mergeId ty il ir
          (STRING sl, STRING sr) -> mergeString ty sl sr
          (ID i, STRING s)       -> mergeString ty i s
          (STRING s, ID i)       -> mergeString ty s i
          (LIST xs, LIST vs)     -> mergeList ty xs vs
          (OBJECT p, OBJECT v)   -> mergeObject ty p v
          (MERGE ml mr, _) ->
              let ll' = merging ty ml mr in
              loop ll' rr
          (_, MERGE ml mr) ->
              let rr' = merging ty ml mr in
              loop ll rr'
          _ -> AST (MERGE ll rr) ty

--------------------------------------------------------------------------------
mergeId :: Typed ->Text -> Text -> AST Typed
mergeId ty x y = AST (ID (x `append` " " `append` y)) ty

--------------------------------------------------------------------------------
mergeString :: Typed -> Text -> Text -> AST Typed
mergeString ty x y = AST (STRING (x `append` " " `append` y)) ty

--------------------------------------------------------------------------------
mergeList :: Typed -> [AST Typed] -> [AST Typed] -> AST Typed
mergeList ty xs vs = AST (LIST (xs ++ vs)) ty

--------------------------------------------------------------------------------
mergeObject :: Typed -> [Prop AST Typed] -> [Prop AST Typed] -> AST Typed
mergeObject ty ps vs = AST (OBJECT (nubBy go (vs ++ ps))) ty where
  go p p' = (propName p) == (propName p')
