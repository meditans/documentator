{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module Documentator where

import           Language.Haskell.Exts.Parser (fromParseResult, parseDecl,
                                               parseType)
import           Language.Haskell.Exts.Syntax hiding (ModuleName)
import           Language.Haskell.Interpreter

import           Control.Lens                 hiding (contains)
import           Control.Lens.Plated
import           Data.Data.Lens               (uniplate)

import           Data.List

-- | A name of a module, like "Control.Lens"
type ModuleName' = String

-- | This functions extracts the names and types. TODO
-- simpleQuery :: ModuleName' -> Interpreter ()
-- functions :: ModuleName -> IO (Either InterpreterError [ModuleElem])
-- functions :: [ModuleName] -> IO (Either InterpreterError [String])
functions ms = runInterpreter $ do
  setImports ("Prelude":ms)
  exports <- concat <$> mapM getModuleExports ms
  let functionsNames = map prettyPrint $ filter isFunction exports
  functionsWithTypes <- mapM addType functionsNames
  return functionsWithTypes

datas ms = runInterpreter $ do
  setImports ("Prelude":ms)
  exports <- concat <$> mapM getModuleExports ms
  let ds = filter isData exports
  return ds

classes ms = runInterpreter $ do
  setImports ("Prelude":ms)
  exports <- concat <$> mapM getModuleExports ms
  let cs = map prettyPrint $ filter isClass exports
  return cs

varyingModules = [ "Control.Varying"
                 , "Control.Varying.Core"
                 , "Control.Varying.Event"
                 , "Control.Varying.Spline"
                 , "Control.Varying.Time"
                 , "Control.Varying.Tween" ]

addType :: MonadInterpreter m => String -> m String
addType s = do
    typeOfS <- typeOf s
    (return . map replaceNewLine) (s ++ " :: " ++ typeOfS)
  where
    replaceNewLine '\n' = ' '
    replaceNewLine x    = x

prettyPrint :: ModuleElem -> String
prettyPrint (Fun x)     = x
prettyPrint (Class x _) = x
prettyPrint (Data x _)  = x

isFunction :: ModuleElem -> Bool
isFunction (Fun _) = True
isFunction _       = False

isData :: ModuleElem -> Bool
isData (Data _ _) = True
isData _          = False

isClass :: ModuleElem -> Bool
isClass (Class _ _) = True
isClass _ = False

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)


-- Parsing part
-- use the function "parseType" in "Language.Haskell.Exts.Parser"

-- What are the building blocks we are going to build on?
-- An example should be type constructors!

------------------
-- example function
-- tween :: (Fractional t, Monad m, Ord t) => Easing t -> t -> t -> t -> SplineT t t m t

-- Come devo parsare questa funzione?
-- Mi interessa individuare le classi ed i punti salienti
-- ad esempio, le classi che non sono dentro prelude, e in questo caso i costruttori di tipo

----------- Plated instance for Type
instance Plated Type where
  plate = uniplate

simpleType1 = fromParseResult $ parseType "Step b c -> Event c"

isTyCon :: Type -> Bool
isTyCon (TyCon t) = True
isTyCon _         = False

_TyFun :: Prism' Type (Type,Type)
_TyFun = prism (\(t1,t2) -> TyFun t1 t2) f
  where
    f (TyFun t1 t2) = Right (t1,t2)
    f x             = Left x


-- This function takes a string like: "stepResult :: Step b c -> Event c"
-- and returns a list of type constructors.
findTyCon :: Type -> [Type]
findTyCon = filter isTyCon . universe

myType :: String -> Type
myType s = ty
  where (TypeSig _ _ ty) = fromParseResult $ parseDecl s

varyingFunctions :: IO [String]
varyingFunctions = do
  Right f <- functions varyingModules
  return . map head . group . sort $ f

pattern KnownType s = TyCon (UnQual (Ident s))

isContainedIn :: Type -> Type -> Bool
isContainedIn s t = s `elem` findTyCon t

prettyVaryingContains t = do
  fs <- varyingFunctions
  mapM_ print $ filter ((t `isContainedIn`) . myType) fs

prettyVaryingResultContains t = do
  fs <- varyingFunctions
  mapM_ print $ filter (\f -> t `isContainedIn` result (myType f)) fs

containsNot :: [Type] -> Type -> Bool
containsNot ts t = all (\s -> not (s `isContainedIn` t)) ts

contains :: [Type] -> Type -> Bool
contains ts t = any (`elem` findTyCon t) ts

----------------- Find the result type of a function

result :: Type -> Type
result = transform (\x -> case x of
                       (TyForall _ _ y) -> y
                       (TyFun _ y)      -> y
                       y                -> y)

-- arguments :: Type -> [Type]
-- arguments = undefined

arguments :: Type -> [Type]
arguments (TyForall _ _ t) = arguments t
arguments (TyFun t1 t2)    = t1 : arguments t2
arguments t                = []

insideAbstractions :: [Type]
insideAbstractions = [KnownType "VarT", KnownType "SplineT", KnownType "Easing"]

test :: IO ()
test = do
  f <- varyingFunctions
  mapM_ print $ filter (containsNot insideAbstractions . result . myType) f

test2 = do
  f <- varyingFunctions
  mapM_ print $ filter (all (containsNot insideAbstractions) . arguments . myType) f

functionsByProp p = do
  f <- varyingFunctions
  mapM_ print $ filter p f

prop1 :: String -> Bool
prop1 f =
    all (containsNot insideAbstractions) (arguments tf) &&
    contains insideAbstractions (result tf)
  where
    tf = myType f

prop2 f = any (contains [KnownType "SplineT"]) (arguments tf) && containsNot insideAbstractions (result tf)
  where tf = myType f

destructor s = functionsByProp $ \f ->
  let tf = myType f
  in    any (contains [KnownType s]) (arguments tf)
     && containsNot insideAbstractions (result tf)

-- constructor returns all the constructors for the term.
constructor :: String -> IO ()
constructor s =
  functionsByProp $
  \f ->
    let tf = myType f
    in all (containsNot [KnownType s])
           (arguments tf) &&
       contains [KnownType s]
                (result tf)

primitiveConstructor :: String -> IO ()
primitiveConstructor s =
  functionsByProp $
  \f ->
    all (containsNot insideAbstractions)
        (arguments $ myType f) &&
    contains [KnownType s]
             (result $ myType f)
