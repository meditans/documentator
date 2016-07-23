{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Lucid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as IO
import Web.Browser

main :: IO ()
main = do report <- pure generateReport
          IO.writeFile "/tmp/report.html" report
          putStrLn "generated /tmp/report.html"
          openBrowser "file:///tmp/report.html"
          return ()

generateReport :: T.Text
generateReport = renderText html 

html :: Html ()
html = table_ (tr_ (td_ (p_ "Hello, World!")))
