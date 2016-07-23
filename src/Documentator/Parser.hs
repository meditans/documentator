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

isTypeSig :: Decl -> Bool
isTypeSig (TypeSig _ _ _) = True
isTypeSig _               = False

------------------------------------------------------------- Extractors

typeSignaturesExtractor :: Extractor [Decl]
typeSignaturesExtractor = filter isTypeSig . declarations
  where
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
