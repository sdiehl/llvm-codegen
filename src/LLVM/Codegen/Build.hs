module LLVM.Codegen.Build where

import LLVM.Codegen
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Execution

import Control.Monad.State

-- -------------------------------------------------------------------------------
-- The "Very High Level API"
-------------------------------------------------------------------------------

-- Take a simple LLVM module, and render it's IR to a file.
logSimple :: FilePath -> LLVM a -> IO String
logSimple ofile mod = do
  let ast = runLLVM (emptyModule "") mod
  out <- runPipeline_ [noopStage] defaultSettings ast
  writeFile ofile out
  return out

logOptSimple :: FilePath -> Int -> LLVM a -> IO String
logOptSimple ofile n mod = do
  let ast = runLLVM (emptyModule "") mod
  out <- runPipeline_ [optimizeStage n] defaultSettings ast
  writeFile ofile out
  return out

-- Take a simple LLVM module, an run the execution function in the context.
execSimple :: Exec a -> LLVM a -> IO String
execSimple run m = do
  let ast = runLLVM (emptyModule "") m
  out <- runPipeline_ [execStage run] defaultSettings ast
  return out
