{-# LANGUAGE DeriveGeneric, ExtendedDefaultRules, OverloadedStrings #-}

module Main where

import           Control.Monad            (unless)
import           Data.Either              (isLeft, lefts, rights)
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.String
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as IO
import           Documentator.Descriptors
import           Documentator.Parser
import           Documentator.Types
import           Documentator.Utils

import Control.Arrow                   ((&&&))
import Language.C.Preprocessor.Remover (getLibExposedModulesPath)
import Lucid
import Options.Generic
import System.Directory                (doesFileExist)
import System.Exit                     (exitFailure)
import Web.Browser

import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax

main :: IO ()
main = do
  path <- getRecord "Documentator"
  exists <- doesFileExist path
  unless exists $ do
    putStrLn $ path ++ " does not exist"
    exitFailure
  modulePaths <- filter (not . isInfixOf "Internal") <$> getLibExposedModulesPath path
  parsedModules <- mapM myParse modulePaths
  -- let parsedModulesWithPath = zip modulePaths parsedModules
  -- if any isLeft parsedModules
  --   then do
  --     putStrLn "I cannot parse the following modules: "
  --     -- mapM_ print (lefts parsedModules)
  --     let errors = filter (isLeft . snd) parsedModulesWithPath
  --     mapM_ print errors
  --     writeFile "tmpParsed" $ show $ head errors
  --   else putStrLn "All modules parsed"
  let mostUsedTypes = count . foldMap componentsExtractor $ rights parsedModules
      report = generateReport (reverse . sortBy (comparing snd) $ mostUsedTypes)
  IO.writeFile "/tmp/report.html" report
  openBrowser "file:///tmp/report.html"
  return ()

generateReport :: Counted (Bare Type) -> T.Text
generateReport = renderText . html

type Counted a = [(a, Int)]

html :: Counted (Bare Type) -> Html ()
html mostUsedTypes =
  html_(
    head_ [] (
      title_ "Automatic Documentation Generator" <>
      link_ [rel_ "stylesheet"
            , href_ "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"]
      <> link_ [rel_ "stylesheet"
            , href_ "https://cdnjs.cloudflare.com/ajax/libs/normalize/3.0.3/normalize.css"
            ]
      <> link_ [rel_ "stylesheet"
        , href_ "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.1.0/milligram.css"
        ]
    ) <>
    body_ (
      section_ [class_ "container"](
        h2_ "Most used component in types"
          <> table_ ( tr_ (th_ "No. of usages" <> th_ "Type Name" <> th_ "Raw type") <> rows)
      )
    )
  )
  where rows = mconcat $ map row mostUsedTypes

row :: (Bare Type, Int) -> Html ()
row (t, num) = tr_ (td_ typeNum <> td_ typeDesc <> td_ (fromString $ show t))
    where typeDesc = fromString (prettyPrint t)
          typeNum  = fromString (show num)
