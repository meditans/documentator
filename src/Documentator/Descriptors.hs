module Documentator.Descriptors where

import Language.Haskell.Exts.Annotated.Syntax
import Documentator.Types

allTyCon :: Located Type -> [Located QName]
allTyCon (TyForall _ _ _ _) = []
allTyCon (TyFun _ t1 t2) = allTyCon t1 ++ allTyCon t2
allTyCon (TyTuple _ _ ts) = concatMap allTyCon ts
allTyCon (TyList _ t) = allTyCon t
allTyCon (TyParArray _ t) = allTyCon t
allTyCon (TyApp _ t1 t2) = allTyCon t1 ++ allTyCon t2
allTyCon (TyVar _ _) = []
allTyCon (TyCon _ qn) = [qn]
allTyCon (TyParen _ t) = allTyCon t
allTyCon (TyInfix _ t1 qn t2) = qn : (allTyCon t1 ++ allTyCon t2)
allTyCon (TyKind _ t _) = allTyCon t
allTyCon (TyPromoted _ _) = []
allTyCon (TyEquals _ _ _) = []
allTyCon (TySplice _ _) = []
allTyCon (TyBang _ _ t) = allTyCon t
allTyCon (TyWildCard _ _) = []

allTypes :: Located Type -> [Located Type]
allTypes (TyForall _ _ _ t) = allTypes t
allTypes (TyFun _ t1 t2) = allTypes t1 ++ allTypes t2
allTypes t@(TyTuple _ _ _) = [t]
allTypes t@(TyList _ _) = [t]
allTypes t@(TyParArray _ _) = [t]
allTypes t@(TyApp _ _ _) = [t]
allTypes (TyVar _ _) = []
allTypes t@(TyCon _ _) = [t]
allTypes (TyParen _ t) = allTypes t
allTypes t@(TyInfix _ _ _ _) = [t]
allTypes (TyKind _ t _) = allTypes t
allTypes t@(TyPromoted _ _) = [t]
allTypes t@(TyEquals _ _ _) = [t]
allTypes t@(TySplice _ _) = [t]
allTypes t@(TyBang _ _ _) = [t]
allTypes t@(TyWildCard _ _) = [t]

resultTyCon :: Located Type -> Located Type
resultTyCon (TyForall _ _ _ t) = resultTyCon t
resultTyCon (TyFun _ _ t) = resultTyCon t
resultTyCon t@(TyTuple _ _ _) = t
resultTyCon t@(TyList _ _) = t
resultTyCon t@(TyParArray _ _) = t
resultTyCon t@(TyApp _ _ _) = t
resultTyCon t@(TyVar _ _) = t
resultTyCon t@(TyCon _ qn) = t
resultTyCon (TyParen _ t) = resultTyCon t
resultTyCon t@(TyInfix _ _ _ _) = t
resultTyCon (TyKind _ t _) = resultTyCon t
resultTyCon t@(TyPromoted _ _) = t
resultTyCon t@(TyEquals _ _ _) = t
resultTyCon t@(TySplice _ _) = t
resultTyCon t@(TyBang _ _ _) = t
resultTyCon t@(TyWildCard _ _) = t

argumentsTyCon :: Located Type -> [Located Type]
argumentsTyCon (TyForall _ _ _ t) = argumentsTyCon t
argumentsTyCon (TyFun _ t1 t2) = [t1] ++ (if isTyFun t2 then argumentsTyCon t2 else [])
argumentsTyCon t@(TyTuple _ _ _) = [t]
argumentsTyCon t@(TyList _ _) = [t]
argumentsTyCon t@(TyParArray _ _) = [t]
argumentsTyCon t@(TyApp _ _ _) = [t]
argumentsTyCon (TyVar _ _) = []
argumentsTyCon t@(TyCon _ _) = [t]
argumentsTyCon (TyParen _ t) = argumentsTyCon t
argumentsTyCon t@(TyInfix _ _ _ _) = [t]
argumentsTyCon (TyKind _ t _) = argumentsTyCon t
argumentsTyCon t@(TyPromoted _ _) = [t]
argumentsTyCon t@(TyEquals _ _ _) = [t]
argumentsTyCon t@(TySplice _ _) = [t]
argumentsTyCon t@(TyBang _ _ _) = [t]
argumentsTyCon t@(TyWildCard _ _) = [t]

isTyFun :: Located Type -> Bool
isTyFun (TyFun _ _ _) = True
isTyFun _           = False
