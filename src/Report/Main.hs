{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IO
import Web.Browser
import Data.Monoid
import Documentator.Types
import Language.Haskell.Exts.Annotated
import Data.String
import Documentator.Parser
import Documentator.Utils      (lensFileExample)


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

type TopUsedTypes = [(Located Type, Int)]

html :: TopUsedTypes -> Html ()
html topUsedTypes = h1_ "Automatic DOC generator" <>
  h2_ "Top used types"
    <> table_ ( tr_ (th_ "No." <> th_ "Type Name") <> rows)
  where rows = mconcat $ map row topUsedTypes


row :: (Located Type, Int) -> Html ()
row (t, num) = tr_ (td_ typeNum <> td_ typeDesc)
    where typeDesc = fromString (prettyPrint (fmap fst t))
          typeNum = fromString $ show num
