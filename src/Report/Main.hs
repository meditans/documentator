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


main :: IO ()
main = do let topUsedTypes = []
          
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
    <> table_ ( tr_ (th_ "Type Name" <> th_ "N. of Usages") <> rows)
  where rows = mconcat $ map row topUsedTypes


row :: (Located Type, Int) -> Html ()
row (t, num) = td_ firstColumn <> td_ secondColumn
    where firstColumn = fromString (prettyPrint (fmap fst t))
          secondColumn = fromString $ show num
        


          
      
