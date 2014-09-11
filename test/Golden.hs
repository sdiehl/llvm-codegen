{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Test.Tasty.HUnit

import Test.Tasty.Golden
import Test.Tasty.Golden.Manage

import Test.Tasty hiding (defaultMain)

import LLVM.Codegen
import qualified LLVM.Codegen.Intrinsics as I

import LLVM.Codegen
import LLVM.Codegen.Build
import LLVM.Codegen.Builder

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
    arg "x"

  def "bar" i32 [(i32, "x")] $
    return $ cons $ ci32 1000

testIf :: LLVM ()
testIf = do
  def "foo" i32 [(i1, "x")] $ do
    x <- arg "x"
    let tr = return $ constant i32 1
    let fl = return $ constant i32 2
    ife i32 x tr fl

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

testWhile :: LLVM ()
testWhile = do
  foo <- external i32 "foo" []

  def "whileloop" i32 [] $ do
    while true $ do
      call (fn foo) []
    return zero

testRange :: LLVM ()
testRange = do
  foo <- external i32 "foo" [(i32, "i")]

  def "rangeloop" i32 [] $ do
    let start = constant i32 0
    let stop  = constant i32 100
    range "i" start stop $ \i -> do
      call (fn foo) [i]
    return zero

testRecord :: LLVM ()
testRecord = do
  rec <- record "myrecord" [("kirk", i32), ("spock", f32)]
  def "main" i32 [] $ do
    x <- allocaRecord rec
    xp <- x `proj` "kirk"
    load xp

testRecordRecord :: LLVM ()
testRecordRecord = do
  reca <- record "a" [("i", i32)]
  recb <- record "b" [("j", reclltype reca)]

  def "main" i32 [] $ do
    vala <- initrec reca [constant i32 1]
    valb <- initrec recb [recvalue vala]
    j <- valb `proj` "j"
    i <- (asRec reca j) `proj` "i"
    return i

testComparison :: LLVM ()
testComparison =
  def "main" i1 [(i32, "x")] $ do
    x <- getvar "x"
    xv <- load x
    xv `ult` one

testIntrinsic :: LLVM ()
testIntrinsic = do
  llsqrt <- llintrinsic I.sqrt

  def "main" f64 [] $ do
    let x = constant f64 2
    call (fn llsqrt) [x]

testPrintf :: LLVM ()
testPrintf =
  def "main" i32 [] $
    printf "%i" [x]
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
        printf "%i" [sum]
    return zero

  where
    inc = add one
    cond x = x `ult` constant i32 15

testLoopnest :: LLVM ()
testLoopnest = do
  foo <- external i32 "foo" [(i32, "i"), (i32, "j")]

  def "main" i32 [] $ do
    loopnest [0,0] [10,20] [1,1] $ \ivars ->
      call (fn foo) ivars
    return zero

testPair :: LLVM ()
testPair = do
  def "main" i32 [] $ do
    tup <- mkPair i32 f32 a b
    l <- inl tup
    r <- inr tup
    printf "%i" [l]
    printf "%f" [r]
  where
     a = constant i32 100
     b = constant f32 200

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

gfunctions = [
    (testSimple     , "simple"),
    (testMultiple   , "multiple"),
    (testIf         , "if"),
    (testFor        , "for"),
    (testWhile      , "while"),
    (testRange      , "range"),
    (testRecord     , "record"),
    (testComparison , "comparison"),
    (testIntrinsic  , "intrinsic"),
    (testPrintf     , "printf"),
    (testFull       , "full"),
    (testLoopnest   , "loopnest"),
    (testPair       , "pair")
  ]

mkTest :: (LLVM a, TestName) -> TestTree
mkTest (fn, tname) =
  goldenVsFileDiff
    tname
    diff
    expected
    output
    (logSimple_ output fn)
  where
    expected     = "test/expected/" ++ tname ++ ".ll.expected"
    output       = "test/output/" ++ tname ++ ".ll"
    diff ref new = ["diff", ref, new]

main :: IO ()
main = do
  let tests = map mkTest gfunctions
  defaultMain $ testGroup "Golden tests" tests
