{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Documentator.Types where

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

-- | This located type has been defined because it's the output of the function
-- `parseContentWithComments`. For something to be located it means to have a
-- definite position in the file (the `SrcSpanInfo`) and some comments
-- associated.
type Located a = a (SrcSpanInfo, [Comment])

-- | Sometimes we don't care for the SrcSpanInfo, and we only retain the comments/
type Commented a = a [Comment]

-- | Some other times, we don't care neither about the comments, and we just
-- want to mantain the thing without metadata.
type Bare a = a ()

-- | An extractor is a function that gets something from the result of
-- `parseContentWithComments`
type Extractor a = Located Module -> a

forgetLocations :: Functor a => Located a -> Commented a
forgetLocations = fmap snd

clean :: Functor f => f a -> Bare f
clean = fmap (const ())
