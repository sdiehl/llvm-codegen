module LLVM.Codegen.Build (
  printSimple,
  logSimple,
  logSimple_,
  logOptSimple,
  execSimple,
  execTest
) where

import LLVM.Codegen
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Execution

-- -------------------------------------------------------------------------------
-- The "Very High Level API"
-------------------------------------------------------------------------------

printSimple :: LLVM a -> IO ()
printSimple mod = do
  let ast = runLLVM (emptyModule "simple module") mod
  out <- runPipeline_ [noopStage] defaultSettings ast
  {-out <- runPipeline_ [verifyStage, optimizeStage 3] defaultSettings ast-}
  putStrLn out


-- Take a simple LLVM module, and render it's IR to a file.
logSimple :: FilePath -> LLVM a -> IO String
logSimple ofile mod = do
  let ast = runLLVM (emptyModule "simple module") mod
  out <- runPipeline_ [verifyStage] defaultSettings ast
  writeFile ofile out
  return out

logSimple_ :: FilePath -> LLVM a -> IO ()
logSimple_ ofile mod = do
  let ast = runLLVM (emptyModule "simple module") mod
  out <- runPipeline_ [verifyStage] defaultSettings ast
  writeFile ofile out

logOptSimple :: FilePath -> Int -> LLVM a -> IO String
logOptSimple ofile n mod = do
  let ast = runLLVM (emptyModule "simple module") mod
  out <- runPipeline_ [verifyStage, optimizeStage n] defaultSettings ast
  writeFile ofile out
  return out

-- Take a simple LLVM module, an run the execution function in the context.
execSimple :: Exec a -> LLVM a -> IO String
execSimple run m = do
  let ast = runLLVM (emptyModule "simple module") m
  out <- runPipeline_ [verifyStage, execStage run] defaultSettings ast
  return out

execTest :: FilePath -> Exec t -> LLVM a -> IO ()
execTest ofile test m = do
  let ast = runLLVM (emptyModule "simple module") m
  out <- runPipeline_ [verifyStage, execStage test] defaultSettings ast
  writeFile ofile out
  return ()
