{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Documentator.Utils where

import qualified Data.Set as Set
import Documentator.Types

import Control.Lens.TH
import Control.Arrow

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
count = map (head &&& length) . List.group . List.sort

unwrapParseOk :: ParseResult t -> Either String t
unwrapParseOk (ParseOk a) = Right a
unwrapParseOk (ParseFailed loc str) = Left $ "Something went wrong in the parser at " ++ show loc ++ "\nThe error was " ++ str

isParseOk :: ParseResult t -> Bool
isParseOk (ParseOk a) = True
isParseOk _ = False

lensCabalFile, lensFileExample :: IO FilePath
lensCabalFile   = getDataFileName "data/lens.cabal"
lensFileExample = getDataFileName "data/Lens.hs"

--------------------------------------------------------------------------------
-- Additional lenses
--------------------------------------------------------------------------------

makePrisms ''Type