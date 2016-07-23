module Documentator.Descriptors where

import Language.Haskell.Exts.Syntax

allTyCon :: Type -> [QName]
allTyCon (TyForall _ _ _) = []
allTyCon (TyFun t1 t2) = allTyCon t1 ++ allTyCon t2
allTyCon (TyTuple _ ts) = concatMap allTyCon ts
allTyCon (TyList t) = allTyCon t
allTyCon (TyParArray t) = allTyCon t
allTyCon (TyApp t1 t2) = allTyCon t1 ++ allTyCon t2
allTyCon (TyVar _) = []
allTyCon (TyCon qn) = [qn]
allTyCon (TyParen t) = allTyCon t
allTyCon (TyInfix t1 qn t2) = qn : (allTyCon t1 ++ allTyCon t2)
allTyCon (TyKind t _) = allTyCon t
allTyCon (TyPromoted _) = []
allTyCon (TyEquals _ _) = []
allTyCon (TySplice _) = []
allTyCon (TyBang _ t) = allTyCon t
allTyCon (TyWildCard _) = []

allTypes :: Type -> [Type]
allTypes (TyForall _ _ t) = allTypes t
allTypes (TyFun t1 t2) = allTypes t1 ++ allTypes t2
allTypes t@(TyTuple _ _) = [t]
allTypes t@(TyList _) = [t]
allTypes t@(TyParArray _) = [t]
allTypes t@(TyApp _ _) = [t]
allTypes (TyVar _) = []
allTypes t@(TyCon _) = [t]
allTypes (TyParen t) = allTypes t
allTypes t@(TyInfix _ _ _) = [t]
allTypes (TyKind t _) = allTypes t
allTypes t@(TyPromoted _) = [t]
allTypes t@(TyEquals _ _) = [t]
allTypes t@(TySplice _) = [t]
allTypes t@(TyBang _ _) = [t]
allTypes t@(TyWildCard _) = [t]


resultTyCon :: Type -> Type
resultTyCon (TyForall _ _ t) = resultTyCon t
resultTyCon (TyFun _ t) = resultTyCon t
resultTyCon t@(TyTuple _ _) = t
resultTyCon t@(TyList _) = t
resultTyCon t@(TyParArray _) = t
resultTyCon t@(TyApp _ _) = t
resultTyCon t@(TyVar _) = t
resultTyCon t@(TyCon qn) = t
resultTyCon (TyParen t) = resultTyCon t
resultTyCon t@(TyInfix _ _ _) = t
resultTyCon (TyKind t _) = resultTyCon t
resultTyCon t@(TyPromoted _) = t
resultTyCon t@(TyEquals _ _) = t
resultTyCon t@(TySplice _) = t
resultTyCon t@(TyBang _ _) = t
resultTyCon t@(TyWildCard _) = t

argumentsTyCon :: Type -> [Type]
argumentsTyCon (TyForall _ _ t) = argumentsTyCon t
argumentsTyCon (TyFun t1 t2) = [t1] ++ (if isTyFun t2 then argumentsTyCon t2 else [])
argumentsTyCon t@(TyTuple _ _) = [t]
argumentsTyCon t@(TyList _) = [t]
argumentsTyCon t@(TyParArray _) = [t]
argumentsTyCon t@(TyApp _ _) = [t]
argumentsTyCon (TyVar _) = []
argumentsTyCon t@(TyCon _) = [t]
argumentsTyCon (TyParen t) = argumentsTyCon t
argumentsTyCon t@(TyInfix _ _ _) = [t]
argumentsTyCon (TyKind t _) = argumentsTyCon t
argumentsTyCon t@(TyPromoted _) = [t]
argumentsTyCon t@(TyEquals _ _) = [t]
argumentsTyCon t@(TySplice _) = [t]
argumentsTyCon t@(TyBang _ _) = [t]
argumentsTyCon t@(TyWildCard _) = [t]

isTyFun :: Type -> Bool
isTyFun (TyFun _ _) = True
isTyFun _           = False
