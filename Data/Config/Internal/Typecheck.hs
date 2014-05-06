{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
--------------------------------------------------------------------------------
-- |
-- Module : Data.Config.Internal.Typecheck
-- Copyright : (C) 2014 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Data.Config.Internal.Typecheck (typecheck) where

--------------------------------------------------------------------------------
import Control.Exception
import Data.Foldable
import Data.Typeable

--------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Data.Text (Text, unpack)

--------------------------------------------------------------------------------
import Data.Config.Internal.AST
import Data.Config.Internal.Scoped
import Data.Config.Internal.Typed

--------------------------------------------------------------------------------
type TyM a = State TyState a
type Types = M.Map Text Type

--------------------------------------------------------------------------------
data TyState
    = TyState
      { _tyVarId   :: !Int
      , _tyConstrs :: ![Constraint]
      , _tyErrors  :: ![TyError]
      , _tyMap     :: !Types
      }

--------------------------------------------------------------------------------
data Constraint
    = Equality (AST Scoped) (AST Scoped) Type Type
    | Existence (AST Scoped) Text

--------------------------------------------------------------------------------
data TyError
    = Mismatch (AST Scoped) Type Type
    | Undefined (AST Scoped) Text deriving Typeable

--------------------------------------------------------------------------------
newtype TypecheckError = TypecheckError [TyError] deriving Typeable

--------------------------------------------------------------------------------
instance Show TyError where
    show (Mismatch s etyp ctyp)
        = prop           ++
          ":"            ++
          show pos       ++
          " Expecting "  ++
          show etyp      ++
          " but having " ++
          show ctyp
      where
        prop = unpack $ scopeProp $ astTag s
        pos  = scopePos  $ astTag s
    show (Undefined s t)
        = prop         ++
          ":"          ++
          show pos     ++
          " Property " ++
          unpack t     ++
          " is undefined"
      where
        prop = unpack $ scopeProp $ astTag s
        pos  = scopePos  $ astTag s

--------------------------------------------------------------------------------
instance Show TypecheckError where
    show (TypecheckError xs)
        = foldMap (\x -> show x ++ "\n") xs

--------------------------------------------------------------------------------
instance Exception TyError

--------------------------------------------------------------------------------
instance Exception TypecheckError

--------------------------------------------------------------------------------
typecheck :: [Prop AST Scoped] -> Either SomeException (Types, [Prop AST Typed])
typecheck ps
    = let (ps', s) = runState (typecheckProps ps) start
          tys      = _tyMap s in
      case _tyErrors s of
          errs | null errs -> Right (tys, ps')
               | otherwise -> Left $ SomeException $ TypecheckError errs
  where
    start
        = TyState
          { _tyVarId   = resetVarId
          , _tyConstrs = []
          , _tyErrors  = []
          , _tyMap     = M.empty
          }

--------------------------------------------------------------------------------
resetVarId :: Int
resetVarId = 1

--------------------------------------------------------------------------------
typecheckProps :: [Prop AST Scoped] -> TyM [Prop AST Typed]
typecheckProps ps
    = do ps' <- mapM typecheckProp ps
         s   <- get
         let constrs = _tyConstrs s

         typecheckConstrs constrs
         return ps'

--------------------------------------------------------------------------------
typecheckConstrs :: [Constraint] -> TyM ()
typecheckConstrs [] = return ()
typecheckConstrs (c:cs)
    = do tys <- gets _tyMap
         case c of
             Equality a b (RefTy t) y ->
                 case M.lookup t tys of
                     Nothing  ->
                         do tyReportError (Undefined a t)
                            typecheckConstrs cs
                     Just typ ->
                         do tyRegisterType t typ
                            typecheckConstrs (Equality a b typ y:cs)
             Equality a b x y@(RefTy _) ->
                 typecheckConstrs (Equality b a y x:cs)
             Equality a b x y ->
                 case checkType a b x y of
                     Right match
                         | match     -> typecheckConstrs cs
                         | otherwise ->
                             do tyReportError (Mismatch a x y)
                                typecheckConstrs cs
                     _ -> error "impossible situation. typecheckConstrs"
             Existence a t ->
                 case M.lookup t tys of
                     Nothing ->
                         do tyReportError (Undefined a t)
                            typecheckConstrs cs
                     _ -> typecheckConstrs cs

--------------------------------------------------------------------------------
typecheckProp :: Prop AST Scoped -> TyM (Prop AST Typed)
typecheckProp (Prop n v)
    = do v' <- typecheckAST v
         let vtype = typedType $ astTag v'

         tyRegisterType n vtype
         tySetVarId resetVarId
         return $ Prop n v'

--------------------------------------------------------------------------------
typecheckAST :: AST Scoped -> TyM (AST Typed)
typecheckAST ast@(AST expr scope)
    = case expr of
    ID i      -> return $ AST (ID i) (Typed (LitTy StringLit) scope)
    STRING s  -> return $ AST (STRING s) (Typed (LitTy StringLit) scope)
    LIST xs   -> typecheckList scope xs
    SUBST s   -> typecheckSubst ast s
    MERGE l r -> typecheckMerge scope l r
    OBJECT ps -> typecheckObject scope ps

--------------------------------------------------------------------------------
typecheckSubst :: AST Scoped -> Text -> TyM (AST Typed)
typecheckSubst ast key
    = do tyAddConstr (Existence ast key)
         return $ AST (SUBST key) (Typed (RefTy key) (astTag ast))

--------------------------------------------------------------------------------
typecheckList :: Scoped -> [AST Scoped] -> TyM (AST Typed)
typecheckList scope []
    = do vid <- tyGetAndIncrVarId
         let typ = ForAllTy vid (TyVarTy vid)
         return $ AST (LIST []) (Typed (AppTy ListTy typ) scope)
typecheckList scope (x:xs)
    = do x'   <- typecheckAST x
         tyxs <- typecheckList scope xs
         let (LIST xs')       = astExpr tyxs
             xtype            = typedType $ astTag x'
             (AppTy _ xstype) = typedType $ astTag tyxs

         typecheckType x (AST (LIST xs) scope) xtype xstype
         return $ AST (LIST (x':xs')) (Typed (AppTy ListTy xtype) scope)

--------------------------------------------------------------------------------
typecheckMerge :: Scoped -> AST Scoped -> AST Scoped -> TyM (AST Typed)
typecheckMerge scope x y
    = do x' <- typecheckAST x
         y' <- typecheckAST y
         let xtype = typedType $ astTag x'
             ytype = typedType $ astTag y'

         typecheckType x y xtype ytype
         return $ AST (MERGE x' y') (Typed xtype scope)

--------------------------------------------------------------------------------
typecheckObject :: Scoped -> [Prop AST Scoped] -> TyM (AST Typed)
typecheckObject scope ps
    = do ps' <- mapM typecheckProp ps
         return $ AST (OBJECT ps') (Typed (LitTy ObjectLit) scope)

--------------------------------------------------------------------------------
typecheckType :: AST Scoped -> AST Scoped -> Type -> Type -> TyM ()
typecheckType a b tyexp typ
    = case checkType a b tyexp typ of
        Left c -> tyAddConstr c
        Right match
            | match     -> return ()
            | otherwise -> tyReportError (Mismatch a tyexp typ)

--------------------------------------------------------------------------------
checkType :: AST Scoped -> AST Scoped -> Type -> Type -> Either Constraint Bool
checkType a b x@(RefTy _) y             = Left (Equality a b x y)
checkType a b x y@(RefTy _)             = Left (Equality a b x y)
checkType _ _ (ForAllTy _ _) _          = Right True
checkType _ _ _ (ForAllTy _ _)          = Right True
checkType a b (AppTy x xs) (AppTy y ys) = checkAppType a b x xs y ys
checkType _ _ x y                       = Right (x == y)

--------------------------------------------------------------------------------
checkAppType :: AST Scoped
             -> AST Scoped
             -> Type
             -> Type
             -> Type
             -> Type
             -> Either Constraint Bool
checkAppType a b x xs y ys
    | x == y    = checkType a b xs ys
    | otherwise = Right False

--------------------------------------------------------------------------------
-- | Utilities
--------------------------------------------------------------------------------
tyUpdateVarId :: (Int -> Int) -> TyState -> TyState
tyUpdateVarId k s
    = let p = _tyVarId s in s { _tyVarId = k p }

--------------------------------------------------------------------------------
tyUpdateErrors :: ([TyError] -> [TyError]) -> TyState -> TyState
tyUpdateErrors k s
    = let p = _tyErrors s in s { _tyErrors = k p }

--------------------------------------------------------------------------------
tyUpdateConstrs :: ([Constraint] -> [Constraint]) -> TyState -> TyState
tyUpdateConstrs k s
    = let p = _tyConstrs s in s { _tyConstrs = k p }

--------------------------------------------------------------------------------
tyUpdateTyMap :: (M.Map Text Type -> M.Map Text Type) -> TyState -> TyState
tyUpdateTyMap k s
    = let p = _tyMap s in s { _tyMap = k p }

--------------------------------------------------------------------------------
tyGetAndIncrVarId :: TyM Int
tyGetAndIncrVarId
    = do i <- gets _tyVarId
         modify (tyUpdateVarId succ)
         return i

--------------------------------------------------------------------------------
tySetVarId :: Int -> TyM ()
tySetVarId i = modify (tyUpdateVarId (const i))

--------------------------------------------------------------------------------
tyAddConstr :: Constraint -> TyM ()
tyAddConstr c = modify (tyUpdateConstrs (c:))

--------------------------------------------------------------------------------
tyReportError :: TyError -> TyM ()
tyReportError e = modify (tyUpdateErrors (e:))

--------------------------------------------------------------------------------
tyRegisterType :: Text -> Type -> TyM ()
tyRegisterType k t = modify (tyUpdateTyMap (M.insert k t))
