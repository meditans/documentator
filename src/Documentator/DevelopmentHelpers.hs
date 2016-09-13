{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Documentator.DevelopmentHelpers where

import Documentator.Types
import Language.Haskell.Exts
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Lens
import Control.Lens.TH

typeFromString :: String -> Either String (Bare Type)
typeFromString s = case parseType s of
    ParseOk annType   -> Right $ fmap (const ()) annType
    ParseFailed _ err -> Left err

makeLensesFor [("extensions", "_extensions")] ''ParseMode

myParseMode :: ParseMode
myParseMode = (_extensions %~ add (EnableExtension MultiParamTypeClasses)) defaultParseMode
  where add = (:)

unsafeTypeFromString :: String -> Bare Type
unsafeTypeFromString (parseTypeWithMode myParseMode -> ParseOk parsed) = clean parsed

exType :: Bare Type
exType =
  unsafeTypeFromString
    "IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a)) "

exTypeWithConstraint :: Bare Type
exTypeWithConstraint =
  unsafeTypeFromString
    "(Foldable f, Indexable i p, Contravariant g, Applicative g) => (s -> f (i, a)) -> Over p g s t a b"

pps :: (Pretty a) => [a] -> IO ()
pps = mapM_ (putStrLn . prettyPrint)

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyPrint

ppset :: (Pretty a) => Set a -> IO ()
ppset = pps . Set.toList

