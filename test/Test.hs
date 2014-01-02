{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Structure
import qualified LLVM.Codegen.Intrinsics as I

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

test_comparison :: LLVM ()
test_comparison = do
  def "main" i1 [(i32, "x")] $ do
    x <- getvar "x"
    xv <- load x
    lt xv one

test_intrinsic :: LLVM ()
test_intrinsic = do
  llsqrt <- llintrinsic I.sqrt
  tixx <- llintrinsic I.tixx

  def "main" f64 [] $ do
    let x = constant f64 2
    call (fn llsqrt) [x]

test_printf :: LLVM ()
test_printf = do
  print <- printf
  fmt <- fixedstr "%i"

  def "main" i32 [] $ do
    fmt' <- gep (global fmt) [zero, zero]
    call (fn print) [fmt', x]
  where
    x = constant i32 42

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

myPipeline :: Pipeline
myPipeline =
  [ verifyPass
  , showPass
  -- , optimizePass 3
  -- , showPass
  ]

compile m = do
  let ast = runLLVM (emptyModule "test module") m
  result <- runPipeline myPipeline defaultSettings ast
  case result of
    Left a -> print a
    Right b -> return ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Pipeline tests"
  [
    testCase "test_simple" $ compile test_simple
  , testCase "test_multiple" $ compile test_multiple
  , testCase "test_for" $ compile test_for
  , testCase "test_record" $ compile test_record
  , testCase "test_comparison" $ compile test_comparison
  , testCase "test_intrinsic" $ compile test_intrinsic
  , testCase "test_printf" $ compile test_printf
  ]
