module Utils (
  compileTest
) where

import LLVM.Codegen
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Execution

-- Test using a single conditional stage on the pipeline.
compileTest :: Check -> LLVM a -> IO ()
compileTest test m = do
  let ast = runLLVM (emptyModule "test module") m
  runPipeline [condPass test] defaultSettings ast
  return ()
