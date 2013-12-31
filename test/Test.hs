module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline

-------------------------------------------------------------------------------
-- Cases
-------------------------------------------------------------------------------

test_simple :: LLVM ()
test_simple = do
  def "foo" i32 [(i32, "x")] $ do
    let a = constant i32 1
    let b = constant i64 1000
    add a b

test_multiple :: LLVM ()
test_multiple = do
  def "foo" i32 [(i32, "x")] $ do
    x <- getvar "x"
    res <- load x
    return $ res

  def "foo" i32 [(i32, "x")] $ do
    return $ cons $ ci32 1000

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

main = defaultMain tests

myPipeline :: Pipeline
myPipeline = [verifyPass, showPass, optimizePass, showPass]

compile m = do
  let ast = runLLVM (emptyModule "test") m
  result <- runPipeline myPipeline defaultSettings ast
  case result of
    Left a -> print a
    Right b -> putStrLn "Compiled!"
  return ()

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Pipeline tests"
  [
    testCase "test_simple" $ compile test_simple
  , testCase "test_multiple" $ compile test_multiple
  ]
