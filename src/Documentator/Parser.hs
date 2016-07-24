{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Parser where

import Preprocessor
import Documentator.Descriptors
import Documentator.Types
import Documentator.Utils

import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Fixity

import Control.Lens
import Data.List
import Data.Ord

myParse :: FilePath -> IO (Module (SrcSpanInfo, [Comment]))
myParse f = associateHaddock . unwrapParseOk . parseFileContentsWithComments parseMode <$> preprocessFile f
  where
    parseMode = defaultParseMode { fixities = Just baseFixities }

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
g :: Extractor a -> IO a
g e = do
  lensFilePath <- lensFileExample
  e <$> myParse lensFilePath

pRaw :: (Show a) => Extractor [a] -> IO ()
pRaw e = g e >>= mapM_ (\a -> print a >> putStrLn "\n")

p :: (Pretty a) => Extractor [a] -> IO ()
p e = g e >>= mapM_ (putStrLn . prettyPrint)

isTypeSig :: Decl (SrcSpanInfo, [Comment]) -> Bool
isTypeSig (TypeSig _ _ _) = True
isTypeSig _               = False

---------------------------------------------------------------- Extractors

typeSignaturesExtractor :: Extractor [Located Decl]
typeSignaturesExtractor = filter isTypeSig . declarations
  where
    declarations :: Module t -> [Decl t]
    declarations (Module _ _ _ _ ds) = ds

typesExtractor :: Extractor [Located Type]
typesExtractor = map getType . filter isTypeSig . typeSignaturesExtractor
  where
    getType (TypeSig _ _ t) = t

instance {-# OVERLAPPING #-} Ord (Located Type) where
  compare t1 t2 = compare (fmap (const ()) t1) (fmap (const ()) t2)

instance {-# OVERLAPPING #-} Eq (Located Type) where
   t1 == t2 = (fmap (const ()) t1) == (fmap (const ()) t2)

instance {-# OVERLAPPING #-} Ord (Located QName) where
  compare qn1 qn2 = compare (fmap (const ()) qn1) (fmap (const ()) qn2)

tyConExtractor :: Extractor [Located QName]
tyConExtractor = ordNub . sort . concatMap allTyCon . ordNub . typesExtractor

allTypesExtractor :: Extractor [Type ()]
allTypesExtractor = concatMap (allTypes . clean) . typesExtractor

typeUsages :: Extractor [(Type (), Int)]
typeUsages =  reverse . sortBy (comparing snd) . count . allTypesExtractor

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

typeFromString :: String -> Either String (Type ())
typeFromString s = case parseType s of
    ParseOk annType -> Right $ fmap (const ()) annType
    ParseFailed _ err -> Left err
