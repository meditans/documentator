{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IO
import Web.Browser
import Data.Monoid
import Documentator.Types
import Data.String
import Documentator.Parser
import Documentator.Utils      (lensFileExample)

import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax

main :: IO ()
main = do
  filePath <- lensFileExample
  mod <- myParse filePath
  let topUsedTypes = typeUsages mod
  report <- pure $ generateReport topUsedTypes
  IO.writeFile "/tmp/report.html" report
  putStrLn "generated /tmp/report.html"
  openBrowser "file:///tmp/report.html"
  return ()

generateReport :: TopUsedTypes -> T.Text
generateReport = renderText . html

type TopUsedTypes = [(Type (), Int)]

html :: TopUsedTypes -> Html ()
html topUsedTypes = h1_ "Automatic DOC generator" <>
  h2_ "Top used types"
    <> table_ ( tr_ (th_ "Type Name" <> th_ "N. of Usages") <> rows)
  where rows = mconcat $ map row topUsedTypes

row :: (Type (), Int) -> Html ()
row (t, num) = tr_ (td_ firstColumn <> td_ secondColumn)
    where firstColumn = fromString (prettyPrint t)
          secondColumn = fromString $ show num
