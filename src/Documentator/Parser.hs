{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Parser where

import Preprocessor
import Documentator.Descriptors
import Documentator.Types
import Documentator.Utils

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Parser

import Control.Lens
import Data.List

myParse :: FilePath -> IO (Module (SrcSpanInfo, [Comment]))
myParse f = associateHaddock . unwrapParseOk . parseFileContentsWithComments parseMode <$> preprocessFile f
  where
    parseMode = defaultParseMode { fixities = Just []
                                 , extensions = defaultExtensions }

-- The parser may fail for the absence of the right extensions. A common trick,
-- used for example by hlint at
-- https://github.com/ndmitchell/hlint/blob/e1c22030721999d4505eb14b19e6f8560a87507a/src/Util.hs
-- is to import all possible reasonable extensions by default. This might be
-- changed in a future version.

defaultExtensions :: [Extension]
defaultExtensions = [e | e@EnableExtension{} <- knownExtensions] \\ map EnableExtension badExtensions

badExtensions =
    [ Arrows -- steals proc
    , TransformListComp -- steals the group keyword
    , XmlSyntax, RegularPatterns -- steals a-b
    , UnboxedTuples -- breaks (#) lens operator
    , QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    , DoRec, RecursiveDo -- breaks rec
    ]

-- g is a convenience function to use in ghci
g :: Extractor b -> IO b
g e = e <$> myParse lensFileExample

pRaw :: (Show a) => Extractor [a] -> IO ()
pRaw e = g e >>= mapM_ (\a -> print a >> putStrLn "\n")

p :: (Pretty a) => Extractor [a] -> IO ()
p e = g e >>= mapM_ (putStrLn . prettyPrint)

isTypeSig :: Decl (SrcSpanInfo, [Comment]) -> Bool
isTypeSig (TypeSig _ _ _) = True
isTypeSig _               = False

---------------------------------------------------------------- Extractors

typeSignaturesExtractor :: Extractor [Decl (SrcSpanInfo, [Comment])]
typeSignaturesExtractor = filter isTypeSig . declarations
  where
    declarations :: Module t -> [Decl t]
    declarations (Module _ _ _ _ ds) = ds

typesExtractor :: Extractor [Type (SrcSpanInfo, [Comment])]
typesExtractor = map getType . filter isTypeSig . typeSignaturesExtractor
  where
    getType (TypeSig _ _ t) = t

instance {-# OVERLAPPING #-} Ord (Type (SrcSpanInfo, [Comment])) where
  compare t1 t2 = compare (fmap (const ()) t1) (fmap (const ()) t2)

instance {-# OVERLAPPING #-} Ord (QName (SrcSpanInfo, [Comment])) where
  compare qn1 qn2 = compare (fmap (const ()) qn1) (fmap (const ()) qn2)

tyConExtractor :: Extractor [Located QName]
tyConExtractor = ordNub . sort . concatMap allTyCon . ordNub . typesExtractor

typeUsages :: Extractor [(Located Type, Int)]
typeUsages =  sort . count . typesExtractor

showTypeUsages :: Extractor [(Located Type, Int)] -> IO ()
showTypeUsages e = g e >>= mapM_ (putStrLn . str)
  where
    str (t, num) = (prettyPrint (fmap fst t)) ++ " " ++ show num

resultTypeExtractor :: Extractor [Located Type]
resultTypeExtractor = map resultTyCon . ordNub . typesExtractor

inputTypesExtractor :: Extractor [Located Type]
inputTypesExtractor = ordNub . concatMap argumentsTyCon . ordNub . typesExtractor

instance {-# OVERLAPPING #-} SrcInfo (SrcSpanInfo, [Comment]) where
  toSrcInfo a b c = (toSrcInfo a b c, [])
  fromSrcInfo a = (fromSrcInfo a, [])
  getPointLoc = getPointLoc . fst
  fileName = fileName . fst
  startLine = startLine . fst
  startColumn = startColumn . fst
