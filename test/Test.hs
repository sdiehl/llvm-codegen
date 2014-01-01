{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Structure

import LLVM.General.AST (Operand)

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

  def "bar" i32 [(i32, "x")] $ do
    return $ cons $ ci32 1000

test_for :: LLVM ()
test_for = do
  foo <- external i32 "foo" []

  def "forloop" i32 [] $ do
    for i inc false $ do
      for j inc false $ do
        call (fn foo) []
    return zero

  where
    i = var i32 zero "i"
    j = var i32 zero "j"
    inc = return one

test_record :: LLVM ()
test_record = do
  rec <- record "mystuct" [("kirk", i32), ("spock", f32)]
  def "main" i32 [] $ do
    alloca (recType rec)
    return one

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
  , testCase "test_for" $ compile test_for
  , testCase "test_record" $ compile test_record
  ]
