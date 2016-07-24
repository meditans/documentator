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
import Documentator.Descriptors
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
html topUsedTypes = 
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
        h2_ "Top used types"
          <> table_ ( tr_ (th_ "No. of usages" <> th_ "Type Name") <> rows)
      )
    )
  )
  where rows = mconcat $ map row topUsedTypes


row :: (Type (), Int) -> Html ()
row (t, num) = tr_ (td_ typeNum <> td_ typeDesc)
    where typeDesc = fromString (prettyPrint t)
          typeNum = fromString $ show num
