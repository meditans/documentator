module Main where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Interpreter

import Documentator.EntryPoint

main :: IO ()
main = do r <- runInterpreter simpleQuery
          case r of
            Left err -> printInterpreterError err
            Right () -> putStrLn "That's all folks"

simpleQuery :: Interpreter ()
simpleQuery = do
  setImportsQ [("Control.Lens", Nothing)]
  exports <- getModuleExports "Control.Lens"
  let functionsNames = map prettyPrint $ filter isFun exports
  functionsWithTypes <- mapM addType functionsNames
  say $ unlines functionsWithTypes

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

prettyPrint :: ModuleElem -> String
prettyPrint (Fun x)      = x
prettyPrint (Class x1 _) = x1
prettyPrint (Data x1 _)  = x1

extractResultType :: Type -> Type
extractResultType (TyForall _ _ t) = extractResultType t
extractResultType (TyFun _ t)      = extractResultType t
extractResultType (TyParen t)      = extractResultType t
extractResultType t                = t

-- | Partial function
expTypeSigToType :: Exp -> Type
expTypeSigToType (ExpTypeSig _ _ t) = t
expTypeSigToType _                  = error "what."
