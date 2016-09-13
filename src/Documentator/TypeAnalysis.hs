{-# LANGUAGE ViewPatterns #-}

module Documentator.TypeAnalysis where

import Documentator.DevelopmentHelpers
import Documentator.Types
import Documentator.Utils
import Language.Haskell.Exts.Syntax

import Control.Lens    hiding (Context)
import Data.Data
import Data.Data.Lens  (uniplate)

import           Data.Set (Set)
import qualified Data.Set as Set

-- | A component is basically a type not in the form a -> b or forall stuff. a
-- -> b
type Component = Type

-- | But is a QName of a constructor
type Constructor = QName

components :: Bare Type -> [Bare Type]
components = filter (not . isBoring) . map reduceContext . components'

components' :: Bare Type -> [Bare Component]
components' (TyForall _ _ c t) = map (TyForall () (Just []) c) (components' t)
components' (TyFun _ t1 t2)    = components' t1 ++ components' t2
components' (TyParen l t)      = components' t
components' t                  = [t]

-- TyForall l (Maybe [TyVarBind l]) (Maybe (Context l)) (Type l)
-- TyFun l (Type l) (Type l)
-- TyTuple l Boxed [Type l]
-- TyList l (Type l)
-- TyParArray l (Type l)
-- TyApp l (Type l) (Type l)
-- TyVar l (Name l)
-- TyCon l (QName l)
-- TyParen l (Type l)
-- TyInfix l (Type l) (QName l) (Type l)
-- TyKind l (Type l) (Kind l)
-- TyPromoted l (Promoted l)
-- TyEquals l (Type l) (Type l)
-- TySplice l (Splice l)
-- TyBang l (BangType l) (Unpackedness l) (Type l)
-- TyWildCard l (Maybe (Name l))
-- TyQuasiQuote l String String

instance Data a => Plated (Type a) where
  plate = uniplate

variables :: Bare Type -> Set (Bare Name)
variables = Set.fromList . toListOf (to universe . traversed . _TyVar . _2)

--------------------------------------------------------------------------------
-- Unuseful constraint checking
--------------------------------------------------------------------------------

isUsefulConstraint :: Bare Type -> Bare Asst -> Bool
isUsefulConstraint t (ClassA _ _ ts) =
  (Set.unions $ map variables ts) `Set.intersection` (variables t) /= Set.empty
isUsefulConstraint _ _ = True

toAssertionList :: Iso' [Bare Asst] (Bare Context)
toAssertionList = iso listToContext contextToList
  where
    contextToList (CxSingle _ a) = [a]
    contextToList (CxTuple _ as) = as
    contextToList (CxEmpty _)    = []
    listToContext []     = CxEmpty ()
    listToContext (a:[]) = CxSingle () a
    listToContext as     = CxTuple () as

reduceContext :: Bare Type -> Bare Type
reduceContext (TyForall _ v (Just c) t) = simplify $ TyForall () v (Just $ filterUseful c) t
  where
    filterUseful = under toAssertionList (filter $ isUsefulConstraint t)
    simplify (TyForall _ _ (Just (CxEmpty _)) t) = t
    simplify t = t
reduceContext t = t

--------------------------------------------------------------------------------
-- Now we'll concentrate on types which are too known to be interesting: we'll
-- call them boring.
--------------------------------------------------------------------------------

isBoring :: Bare Type -> Bool
isBoring (TyVar _ (Ident _ _)) = True
isBoring (TyCon () (UnQual () (Ident () a))) = a `elem` primitiveTypes
isBoring t = False

primitiveTypes = ["Int", "Bool", "String", "Integer", "Double", "Float"]