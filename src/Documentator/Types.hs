{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Documentator.Types where

import Language.Haskell.Exts.Annotated

type Extractor a = Module (SrcSpanInfo, [Comment]) -> a

type Located a = a (SrcSpanInfo, [Comment])

