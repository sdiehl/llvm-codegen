{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.Codegen.Build (
  printSimple,
  logSimple,
  logSimple_,
  logOptSimple,
  execSimple,
  execTest,
  sharedObject
) where

import Data.Data
import System.Exit
import Control.Monad.Except
import Control.Exception

import LLVM.Codegen
import LLVM.Codegen.Utils
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Execution

import LLVM.General.PrettyPrint
import qualified LLVM.General.AST as AST

-------------------------------------------------------------------------------
-- Codegen Error Hoisting
-------------------------------------------------------------------------------

data CodegenErr = CodegenFail String | ExecFail String
   deriving (Show, Typeable)

instance Exception CodegenErr

runLLVMIO :: AST.Module -> LLVM a -> IO (AST.Module)
runLLVMIO mod m = case runLLVM mod m of
  Left err -> do
    throw (CodegenFail err)
    exitFailure
  Right val -> return val

-- -------------------------------------------------------------------------------
-- The "Very High Level API"
-------------------------------------------------------------------------------

defaultModuleName :: String
defaultModuleName = "simple module"

-- Take a simple LLVM module, and render it's IR to a stdout.
printSimple :: LLVM a -> IO ()
printSimple mod = do
  ast <- runLLVMIO (emptyModule defaultModuleName) mod
  {-putStrLn (showPretty ast)-}
  out <- runPipeline_ [verifyStage] defaultSettings ast
  {-out <- runPipeline_ [verifyStage, optimizeStage 3] defaultSettings ast-}
  putStrLn out

-- Take a simple LLVM module, and render it's IR to a file.
logSimple :: FilePath -> LLVM a -> IO String
logSimple ofile mod = do
  ast <- runLLVMIO (emptyModule defaultModuleName) mod
  out <- runPipeline_ [verifyStage] defaultSettings ast
  writeFile ofile out
  return out

logSimple_ :: FilePath -> LLVM a -> IO ()
logSimple_ ofile mod = do
  ast <- runLLVMIO (emptyModule defaultModuleName) mod
  out <- runPipeline_ [verifyStage] defaultSettings ast
  writeFile ofile out

logOptSimple :: FilePath -> Int -> LLVM a -> IO String
logOptSimple ofile n mod = do
  ast <- runLLVMIO (emptyModule defaultModuleName) mod
  out <- runPipeline_ [verifyStage, optimizeStage n] defaultSettings ast
  writeFile ofile out
  return out

-- Take a simple LLVM module, an run the execution function in the context.
execSimple :: Exec a -> LLVM a -> IO String
execSimple run m = do
  ast <- runLLVMIO (emptyModule defaultModuleName) m
  out <- runPipeline_ [verifyStage, execStage run] defaultSettings ast
  return out

execTest :: FilePath -> Exec t -> LLVM a -> IO ()
execTest ofile test m = do
  ast <- runLLVMIO (emptyModule defaultModuleName) m
  out <- runPipeline_ [verifyStage, execStage test] defaultSettings ast
  writeFile ofile out
  return ()

-- Take a simple LLVM module, and compie it to a C shared object for linking with C/C++.
sharedObject :: FilePath -> Exec t -> LLVM a -> IO ()
sharedObject ofile test m = do
  ast <- runLLVMIO (emptyModule defaultModuleName) m
  out <- runPipeline_ [noopStage] defaultSettings ast
  cc <- getCC
  writeFile ofile out
  return ()
