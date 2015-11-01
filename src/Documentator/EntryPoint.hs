{-# LANGUAGE TemplateHaskell #-}

-- | Questo modulo è pensato come un punto d'ingresso per il codice della
-- libreria. Qui ci sono le funzioni per caricare un file attraverso Hint.

module Documentator.EntryPoint where

import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty (prettyPrint)
import Control.Lens
import Control.Lens.Extras (is)

-- | Questa funzione prende tutti gli import di un modulo. È pensata per essere
-- usata come:
--
-- > runInterpreter $ queryModule "Control.Lens"
queryModule :: I.MonadInterpreter m => I.ModuleName -> m [I.ModuleElem]
queryModule mod = do
  I.setImportsQ [(mod, Nothing)]
  I.getModuleExports mod

-- | Funzioni indicatrici di Funzioni, Classi, e Data
-- | Queste funzioni non servono, visto che posso scrivere
-- > is _Fun
isFun, isClass, isData :: I.ModuleElem -> Bool
isFun   (I.Fun _)     = True
isFun   _           = False
isClass (I.Class _ _) = True
isClass _           = False
isData  (I.Data _ _)  = True
isData  _           = False

-- | Funzioni che filtrano Funzioni, Classi, e Data

filterFun, filterClass, filterData :: [I.ModuleElem] -> [I.ModuleElem]
filterFun   = filter isFun
filterClass = filter isClass
filterData  = filter isData

-- | Prende una funzione dal modulo
aFunction :: I.ModuleName -> IO (Either I.InterpreterError I.ModuleElem)
aFunction mod = I.runInterpreter $ (head . filterFun) <$> queryModule mod

addType :: I.MonadInterpreter m => String -> m String
addType s = do
  typeOfS <- I.typeOf s
  (return . map replaceNewLine) (s ++ " :: " ++ typeOfS)
  where
    replaceNewLine '\n' = ' '
    replaceNewLine x    = x

withType :: I.MonadInterpreter m => String -> m (String, String)
withType s = do
  typeOfS <- I.typeOf s
  return $ (s, map replaceNewLine typeOfS)
  where
    replaceNewLine '\n' = ' '
    replaceNewLine x    = x

functionWithType :: I.MonadInterpreter m => String -> I.ModuleName -> m (String,String)
functionWithType fun mod = do
  I.setImportsQ [(mod, Nothing)]
  withType fun


noNewLines :: String -> String
noNewLines = map replaceNewLine
  where
    replaceNewLine '\n' = ' '
    replaceNewLine x    = x

-- Voglio scrivere una funzione che va bene soltanto sui Fun, altrimenti non
-- ritorna niente, usando l'interfaccia delle lenti.

-- Vediamo il tipo di _Left

makePrisms ''I.ModuleElem

-- over _Fun è una funzione che modifica solamente il Fun

-- quindi isFun può essere scritto con un'interfaccia prismatica?

-- is
-- filtered

-- Ora voglio interpretare i tipi

b = preview (_Right._Fun) <$> aFunction "Control.Lens"

tipiFunzioni mod = do
  funs <- toListOf (folded . _Fun) <$> queryModule mod
  tipi <- mapM (\x -> noNewLines <$> I.typeOf x) funs
  return $ map ((getComponents <$>) . parseType) tipi

getComponents :: Type -> [Type]
getComponents (TyForall Nothing ctx t) = map (TyForall Nothing ctx) (getComponents t)
getComponents (TyForall (Just t1) t2 t3) = undefined
getComponents (TyFun t1 t2) = getComponents t1 ++ getComponents t2
getComponents (TyTuple boxed ts) = concatMap getComponents ts
getComponents (TyList t) = getComponents t
getComponents (TyParArray t) = getComponents t
getComponents (TyApp t1 t2) = getComponents t1 ++ getComponents t2
getComponents (TyVar t) = [TyVar t]
getComponents (TyCon t) = [TyCon t]
getComponents (TyParen t) = getComponents t
getComponents (TyInfix t1 t2 t3) = [TyCon t2] ++ getComponents t1 ++ getComponents t3
getComponents (TyKind t k) = getComponents t
getComponents (TyPromoted t) = undefined
getComponents (TyEquals t1 t2) = undefined
getComponents (TySplice t) = undefined
getComponents (TyBang t1 t2) = undefined

-- makePrisms ''ParseResult

comparison f mod = do
  (_, t) <- functionWithType f mod
  I.liftIO $ putStrLn t

  let (ParseOk p) = parseType t --ParseResult
  I.liftIO $ mapM_ (putStrLn . prettyPrint) (getComponents p)
  -- return ()

debugAction :: IO (Either I.InterpreterError ())
debugAction = I.runInterpreter $ comparison "lens" "Control.Lens"
