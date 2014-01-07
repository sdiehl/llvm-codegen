{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Utils

import Foreign.LibFFI
import Foreign.C.Types

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Pipeline
import LLVM.Codegen.Structure
import LLVM.Codegen.Array
import LLVM.Codegen.Execution

import LLVM.General.AST (Operand, Type)

-------------------------------------------------------------------------------
-- Modules
-------------------------------------------------------------------------------

-- saxpy [t] α x y out =
--  out <- α * x + y
--
-- α : Scalar t
-- x : Vector t
-- y : Vector t
axpy :: Type -> LLVM ()
axpy ty =
  def "saxpy" i32
    [ (i32, "n")           -- element count
    , (ty, "a")            -- scalar
    , (pointer ty, "x")    -- vector x
    , (pointer ty, "y")    -- vector y
    , (pointer ty, "out")  -- vector y
    ] $ do

    n <- arg "n"
    a <- arg "a"
    xarr <- arrayArg "x" ty [constant i32 64]
    yarr <- arrayArg "y" ty [constant i32 64]
    oarr <- arrayArg "out" ty [constant i32 64]

    let inc = add one
    let i = avar i32 zero

    for i inc (`ilt` n) $ \ix -> do
      xi <- arrayGet xarr [ix]
      yi <- arrayGet yarr [ix]
      ax <- fmul a xi
      py <- fadd ax yi
      arraySet oarr [ix] py

    return zero

saxpy = axpy f32
daxpy = axpy f64

diag :: LLVM ()
diag = do
  def "diag" i32
    [ (pointer f64, "x") ] $ do

    let n = constant i32 8
    xarr <- arrayArg "x" f64 [constant i32 8, constant i32 8]

    let inc = add one
    let i = avar i32 zero

    for i inc (`ilt` n) $ \ix -> do
      ixf <- uitofp f64 ix
      let val = constant f64 1
      arraySet xarr [ix, ix] val

    return zero

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

diagTest :: Check
diagTest (ctx, m, _) = do
  x <- VM.replicate 64 (0 :: CDouble)
  xptr <- vectorArg x

  callAs ctx m "diag" retVoid [xptr]
  frozen <- V.freeze x
  let output = V.toList frozen
  return $ output == (diag 8)

  where
    diag n = [if i == j then 1 else 0 | i <- [1..n], j <- [1..n]]

daxpyTest :: Check
daxpyTest (ctx, m, _) = do
  x <- VM.replicate 64 (10 :: CDouble)
  y <- VM.replicate 64 (20 :: CDouble)
  o <- VM.replicate 64 (0 :: CDouble)

  xptr <- vectorArg x
  yptr <- vectorArg y
  optr <- vectorArg o

  let args = [argCInt 64, argCDouble 3, xptr, yptr, optr]
  callAs ctx m "saxpy" retVoid args
  let expected = replicate 64 50
  frozen <- V.freeze o
  let output = V.toList frozen
  return $ output == expected

{-saxpyTest :: Check-}
saxpyTest (ctx, m, _) = do
  x <- VM.replicate 64 (10 :: CFloat)
  y <- VM.replicate 64 (20 :: CFloat)
  o <- VM.replicate 64 (0 :: CFloat)

  xptr <- vectorArg x
  yptr <- vectorArg y
  optr <- vectorArg o

  let args = [argCInt 64, argCFloat 3, xptr, yptr, optr]
  callAs ctx m "saxpy" retVoid args
  let expected = replicate 64 50
  frozen <- V.freeze o
  let output = V.toList frozen
  return $ output == expected


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Pipeline tests"
  [
    testCase "test_daxpy" $ compileTest saxpyTest saxpy
  , testCase "test_daxpy" $ compileTest daxpyTest daxpy
  , testCase "test_diag" $ compileTest diagTest diag
  ]
