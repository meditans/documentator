{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Utils where

import qualified Data.Set as Set
import Documentator.Types

import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

import qualified Data.List as List

-- See https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
-- getDataFileName will return the correct path to data file portably
import Paths_documentator (getDataFileName)

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

count :: (Ord a) => [a] -> [(a, Int)]
count = map (\xs@(x:_) -> (x, length xs)) . List.group . List.sort

unwrapParseOk :: ParseResult t -> t
unwrapParseOk (ParseOk a) = a
unwrapParseOk _ = error "Something went wrong in the parser"

lensCabalFile, lensFileExample :: IO FilePath
lensCabalFile   = getDataFileName "data/lens.cabal"
lensFileExample = getDataFileName "data/Lens.hs"
