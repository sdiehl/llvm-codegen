{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Test.Tasty.HUnit

import Test.Tasty.Golden
import Test.Tasty.Golden.Manage

import Test.Tasty hiding (defaultMain)

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Structure
import LLVM.Codegen.Array
import LLVM.Codegen.Execution
import qualified LLVM.Codegen.Intrinsics as I

import LLVM.General.AST (Operand)

-------------------------------------------------------------------------------
-- Cases
-------------------------------------------------------------------------------

testSimple :: LLVM ()
testSimple =
  def "foo" i32 [(i32, "x")] $
    add a b
  where
     a = constant i32 1
     b = constant i64 1000

testMultiple :: LLVM ()
testMultiple = do
  def "foo" i32 [(i32, "x")] $ do
    x <- getvar "x"
    load x

  def "bar" i32 [(i32, "x")] $
    return $ cons $ ci32 1000

testFor :: LLVM ()
testFor = do
  foo <- external i32 "foo" []

  def "forloop" i32 [] $ do
    for i inc (const true) $ \_ ->
      for j inc (const true) $ \_ ->
        call (fn foo) []
    return zero

  where
    i = var i32 zero "i"
    j = var i32 zero "j"
    inc = add one

testRecord :: LLVM ()
testRecord = do
  rec <- record "myrecord" [("kirk", i32), ("spock", f32)]
  def "main" i32 [] $ do
    x <- allocaRecord rec
    xp <- proj rec x "kirk"
    load xp

testComparison :: LLVM ()
testComparison =
  def "main" i1 [(i32, "x")] $ do
    x <- getvar "x"
    xv <- load x
    xv `ilt` one

testIntrinsic :: LLVM ()
testIntrinsic = do
  llsqrt <- llintrinsic I.sqrt

  def "main" f64 [] $ do
    let x = constant f64 2
    call (fn llsqrt) [x]

testDebug :: LLVM ()
testDebug =
  def "main" i32 [] $
    debug "%i" [x]
  where
    x = constant i32 42

testFull :: LLVM ()
testFull =
  def "main" i32 [] $ do
    let i = var i32 zero "i"
    let j = var i32 zero "j"

    for i inc cond $ \ix ->
      for j inc cond $ \jx -> do
        sum <- add ix jx
        debug "%i" [sum]
    return zero

  where
    inc = add one
    cond x = x `ilt` constant i32 15

testLoopnest :: LLVM ()
testLoopnest = do
  foo <- external i32 "foo" [(i32, "i"), (i32, "j")]

  def "main" i32 [] $ do
    loopnest [0,0] [10,20] [1,1] $ \ivars ->
      call (fn foo) ivars
    return zero

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

gfunctions = [
    (testSimple     , "simple"),
    (testMultiple   , "multiple"),
    (testFor        , "for"),
    (testRecord     , "record"),
    (testComparison , "comparison"),
    (testIntrinsic  , "intrinsic"),
    (testDebug      , "debug"),
    (testFull       , "full"),
    (testLoopnest   , "loopnest")
  ]

mkTest :: (LLVM a, TestName) -> TestTree
mkTest (fn, tname) =
  goldenVsFileDiff
    tname
    diff
    expected
    output
    (runTest fn output)
  where
    expected     = "test/expected/" ++ tname ++ ".ll.expected"
    output       = "test/output/" ++ tname ++ ".ll"
    diff ref new = ["diff", ref, new]

main :: IO ()
main = do
  let tests = map mkTest gfunctions
  defaultMain $ testGroup "Golden tests" tests

runTest :: LLVM a -> FilePath -> IO ()
runTest input output = do
  res <- compile input
  writeFile output res

myPipeline :: Pipeline
myPipeline = [
    verifyStage
  {-, optimizeStage 3-}
  ]

compile :: LLVM a -> IO String
compile m = do
  let ast = runLLVM (emptyModule "test module") m
  result <- runPipeline_ myPipeline settings ast
  return (either id id result)
  where
    settings = defaultSettings { verbose = False }
