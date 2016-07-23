{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Parser where

import Preprocessor
import Documentator.Descriptors
import Documentator.Types
import Documentator.Utils

import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

import Control.Lens
import Data.List

myParse :: FilePath -> IO (ParseResult (Module, [Comment], [UnknownPragma]))
myParse f = parseFileContentsWithCommentsAndPragmas defaultParseMode <$> preprocessFile f

myParseOnlyModule :: FilePath -> IO (ParseResult Module)
myParseOnlyModule f = (fmap . fmap) fst3 (myParse f)
  where fst3 (a,_,_) = a

g :: Extractor a -> IO a
g e = e . unwrapParseOk <$> myParseOnlyModule lensFileExample

pRaw :: (Show a) => Extractor [a] -> IO ()
pRaw e = g e >>= mapM_ (\a -> print a >> putStrLn "\n")

p :: (Pretty a) => Extractor [a] -> IO ()
p e = g e >>= mapM_ (putStrLn . prettyPrint)

myParseOnlyDeclarationsAndModuleName :: FilePath -> IO (ParseResult (ModuleName, [Decl]))
myParseOnlyDeclarationsAndModuleName f = (fmap . fmap) extractDecls (myParseOnlyModule f)
  where
    extractDecls (Module _ mn _ _ _ _ ds) = (mn, ds)

prettyPrintDeclarations f = do
  ParseOk (_, ds) <- myParseOnlyDeclarationsAndModuleName f
  mapM_ (\d -> print d >> putStrLn "\n") ds

prettyList :: (Show a, Foldable t) => t a -> IO ()
prettyList = mapM_ (\d -> print d >> putStrLn "\n")

-- We are mainly interested in the type signatures for now
typeSignatures :: FilePath -> IO (ParseResult [Decl])
typeSignatures f = (fmap . fmap) extractSignatures (myParseOnlyDeclarationsAndModuleName f)
  where
    extractSignatures (_, ds) = filter isTypeSig ds
    isTypeSig (TypeSig _ _ _) = True
    isTypeSig _               = False

typeSignaturesExtractor :: Extractor [Decl]
typeSignaturesExtractor = filter isTypeSig . declarations
  where

isTypeSig :: Decl -> Bool
isTypeSig (TypeSig _ _ _) = True
isTypeSig _               = False

declarations :: Module -> [Decl]
declarations (Module _ _ _ _ _ _ ds) = ds

typesExtractor :: Extractor [Type]
typesExtractor = map getType . filter isTypeSig . typeSignaturesExtractor
  where
    getType (TypeSig _ _ t) = t

tyConExtractor :: Extractor [QName]
tyConExtractor = ordNub . sort . concatMap allTyCon . ordNub . typesExtractor

resultTypeExtractor :: Extractor [Type]
resultTypeExtractor = map resultTyCon . ordNub . typesExtractor

-- There is not a Pretty [Type] instance
inputTypesExtractor :: Extractor [Type]
inputTypesExtractor = ordNub . concatMap argumentsTyCon . ordNub . typesExtractor
