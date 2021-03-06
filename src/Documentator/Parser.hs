{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Parser where

import Documentator.Descriptors
import Documentator.Types
import Documentator.Utils
import Documentator.TypeAnalysis
import Language.C.Preprocessor.Remover

import Language.Haskell.Exts
import Language.Haskell.Exts.Fixity
import Language.Haskell.Exts.Parser

import Control.Exception

import Control.Lens
import Data.List

-- | This function, give a filepath, will try to interpret it as a Located
-- Module
myParse :: FilePath -> IO (Either String (Located Module))
myParse f =
  fmap associateHaddock .
  unwrapParseOk . parseFileContentsWithComments parseMode <$>
  preprocessFile f

parseMode = defaultParseMode { fixities = Nothing
                             , extensions = defaultExtensions
                             }

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

isTypeSig :: Located Decl -> Bool
isTypeSig (TypeSig _ _ _) = True
isTypeSig _               = False

--------------------------------------------------------------------------------
-- Common Extractors
--------------------------------------------------------------------------------

-- | This function gets the typesignatures of a module, i.e. those in the form
-- @foo :: Type@
typeSignaturesExtractor :: Extractor [Located Decl]
typeSignaturesExtractor = filter isTypeSig . declarations
  where
    declarations :: Module t -> [Decl t]
    declarations (Module _ _ _ _ ds) = ds

-- | This function gets all the types, i.e. the typesignatures without the name
-- to which the type is bound.
typesExtractor :: Extractor [Located Type]
typesExtractor = map getType . filter isTypeSig . typeSignaturesExtractor
  where
    getType (TypeSig _ _ t) = t


-- | This ord instances are useful because ...
instance {-# OVERLAPPING #-} Ord (Located Type) where
  compare t1 t2 = compare (fmap (const ()) t1) (fmap (const ()) t2)

instance {-# OVERLAPPING #-} Eq (Located Type) where
   t1 == t2 = (fmap (const ()) t1) == (fmap (const ()) t2)

instance {-# OVERLAPPING #-} Ord (Located QName) where
  compare qn1 qn2 = compare (fmap (const ()) qn1) (fmap (const ()) qn2)

tyConExtractor :: Extractor [Located QName]
tyConExtractor = ordNub . sort . concatMap allTyCon . ordNub . typesExtractor

allTypesExtractor :: Extractor [Bare Type]
allTypesExtractor = concatMap (allTypes . clean) . typesExtractor

typeUsages :: Extractor [(Bare Type, Int)]
typeUsages = count . allTypesExtractor

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

typeFromString :: String -> Either String (Bare Type)
typeFromString s = case parseType s of
    ParseOk annType   -> Right $ clean annType
    ParseFailed _ err -> Left err

--------------------------------------------------------------------------------
-- New extractors
--------------------------------------------------------------------------------

bareTypesExtractor :: Extractor [Bare Type]
bareTypesExtractor = map clean . typesExtractor

componentsExtractor :: Extractor [Bare Component]
componentsExtractor = concatMap components . bareTypesExtractor
