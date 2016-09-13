{-# LANGUAGE DeriveGeneric, ExtendedDefaultRules, OverloadedStrings #-}

module Main where

import Documentator.Descriptors
import Documentator.Parser
import Documentator.Types
import Documentator.Utils

import           Control.Arrow    ((&&&))
import           Control.Monad    (unless, void)
import           Data.Either      (rights)
import           Data.List        (isInfixOf, sortBy)
import qualified Data.Map         as M
import           Data.Monoid      ((<>))
import           Data.Ord         (comparing)
import           Data.String      (fromString)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Data.Text.Lazy   (toStrict)
import           System.Directory (doesFileExist, getCurrentDirectory)
import           System.Exit      (exitFailure)
import           System.IO.Temp   (openTempFile)

import Lucid
import Options.Generic (getRecord)
import Web.Browser     (openBrowser)

import Language.C.Preprocessor.Remover (getLibExposedModulesPath)
import Language.Haskell.Exts.Pretty    (prettyPrint)
import Language.Haskell.Exts.Syntax    (Type (..))
import Language.Haskell.Names          (ppSymbol, resolve)

main :: IO ()
main = do
  path <- getRecord "Documentator"

  exists <- doesFileExist path
  unless exists $ do
    putStrLn $ path ++ " does not exist"
    exitFailure

  modulePaths <- filter (not . isInfixOf "Internal") <$> getLibExposedModulesPath path
  parsedModules <- rights <$> mapM myParse modulePaths

  let
    mostUsedTypes = reverse . sortBy (comparing snd) . count . foldMap componentsExtractor $ parsedModules
    typesReport = genericHtml "Most frequently used components in types" (toTable mostUsedTypes)

    environment = resolve parsedModules M.empty
    symbols = concat (M.elems environment)
    symbolsReport = genericHtml "Symbols" $
                    mconcat . map (ul_ . fromString . ppSymbol) $ symbols

  mapM_ display [typesReport, symbolsReport]

display :: Html () -> IO ()
display html = do
  (f, h) <- openTempFile "documentator" "documentator.html"
  T.hPutStrLn h (toStrict . renderText $ html)
  curr <- getCurrentDirectory
  void $ openBrowser $ "file://" ++ curr ++ "/" ++ f

toTable :: Counted (Bare Type) -> Html ()
toTable mostUsedTypes = table_ $ do
  tr_ (th_ "No. of usages" <> th_ "Type Name" <> th_ "Raw type")
  mconcat $ flip map mostUsedTypes $ \(t,num) -> tr_ $ do
    td_ $ fromString (show num)
    td_ $ fromString (prettyPrint t)
    td_ $ fromString (show t)

genericHtml :: Html () -> Html () -> Html ()
genericHtml desc inner = do
  html_ $ do
    head_ [] $ do
      title_ desc
      link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/normalize/3.0.3/normalize.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.1.0/milligram.css"]
    body_ $ do
      section_ [class_ "container"] $ do
        h2_ desc
        p_ inner
