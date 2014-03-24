module Utils (
  -- compileTest,
  execTest,
) where

import LLVM.Codegen
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Execution

-- | Test using a single conditional stage on the pipeline.
-- compileTest :: FilePath -> Check -> LLVM a -> IO ()
-- compileTest ofile test m = do
--   let ast = runLLVM (emptyModule "test module") m
--   out <- runPipeline_ [condStage test] defaultSettings ast
--   writeFile ofile (either id id out)
--   return ()

-- | Test using an execution context yielding the result to a IO monad.
execTest :: FilePath -> Exec t -> LLVM a -> IO ()
execTest ofile test m = do
  let ast = runLLVM (emptyModule "test module") m
  out <- runPipeline_ [execStage test] defaultSettings ast
  writeFile ofile (either id id out)
  return ()
