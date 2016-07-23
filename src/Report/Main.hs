{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IO
import Web.Browser
import Data.Monoid

main :: IO ()
main = do report <- pure generateReport
          IO.writeFile "/tmp/report.html" report
          putStrLn "generated /tmp/report.html"
          openBrowser "file:///tmp/report.html"
          return ()

generateReport :: T.Text
generateReport = renderText html 

html :: Html ()
html = h1_ "Automatic DOC generator" <> 
  h2_ "Top used types" 
    <> table_ ( tr_ (th_ "Type Name" <> th_ "N. of Usages") <>
                     td_ "First Type" <> td_ "1")
  

          
      
