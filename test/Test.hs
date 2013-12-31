module Main where

import LLVM.Codegen
import LLVM.Codegen.Module
import LLVM.Codegen.Logic
import LLVM.Codegen.Types
import LLVM.Codegen.Constant
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline

test_simple :: LLVM ()
test_simple = do
  def "foo" i32 [(i32, "x")] $ do
    let a = cons $ ci32 1
    let b = cons $ ci32 1000
    sum <- add a b
    x <- getvar "x"
    res <- load x
    return $ res

main :: IO ()
main = do
  let ast = runLLVM (emptyModule "test") test_simple
  result <- runPipeline myPipeline defaultSettings ast
  case result of
    Left a -> print a
    Right b -> putStrLn "Compiled!"
  return ()
