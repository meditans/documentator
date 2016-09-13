module Documentator.DataDeclAnalysis where


typesExtractor :: Extractor [Located Type]
typesExtractor = map getDecl . filter isDecl . typeSignaturesExtractor
  where
    getType (TypeSig _ _ t) = t