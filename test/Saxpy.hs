{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Data.Int
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
import LLVM.Codegen.Build

import LLVM.General.AST (Operand, Type)

import Control.Monad.Reader

-------------------------------------------------------------------------------
-- Modules
-------------------------------------------------------------------------------

-- void saxpy(int n, float a, float *x, float *y)
-- {
--   for (int i = 0; i < n; ++i) {
--       y[i] = a*x[i] + y[i];
--   }
-- }

axpy :: Type -> LLVM ()
axpy ty =
  def "axpy" i32
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
    [ (pointer i64, "x") ] $ do

    let n = constant i32 8
    xarr <- arrayArg "x" i64 [n, n]

    let val = constant i64 1

    let inc = add one

    -- iteration var
    let i = avar i32 zero
    for i inc (`ilt` n) $ \ix -> do
      arraySet xarr [ix, ix] val

    return zero

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

diagVec :: Int -> [Int64]
diagVec n = [if i == j then 1 else 0 | i <- [1..n], j <- [1..n]]

diagTest :: Exec Bool
diagTest = do
  x <- liftIO $ VM.replicate (8*8) (0 :: Int64)
  xptr <- liftIO $ vectorArg x

  jitCall_ "diag" [xptr]
  frozen <- liftIO $ V.freeze x

  let output = V.toList frozen
  return $ output == (diagVec 8)

daxpyTest :: Exec Bool
daxpyTest = do
  x <- liftIO $ VM.replicate 64 (10 :: Double)
  y <- liftIO $ VM.replicate 64 (20 :: Double)
  o <- liftIO $ VM.replicate 64 (0 :: Double)

  xptr <- liftIO $ vectorArg x
  yptr <- liftIO $ vectorArg y
  optr <- liftIO $ vectorArg o

  let args = [argCInt 64, argCDouble 3, xptr, yptr, optr]
  let expected = replicate 64 50

  jitCall_ "axpy" args
  frozen <- liftIO $ V.freeze o

  let output = V.toList frozen
  return $ output == expected

saxpyTest :: Exec Bool
saxpyTest = do
  x <- liftIO $ VM.replicate 64 (10 :: Float)
  y <- liftIO $ VM.replicate 64 (20 :: Float)
  o <- liftIO $ VM.replicate 64 (0 :: Float)

  xptr <- liftIO $ vectorArg x
  yptr <- liftIO $ vectorArg y
  optr <- liftIO $ vectorArg o

  let args = [argCInt 64, argCFloat 3, xptr, yptr, optr]
  let expected = replicate 64 50

  jitCall_ "axpy" args
  frozen <- liftIO $ V.freeze o

  let output = V.toList frozen
  return $ output == expected


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [

    testGroup "Pipeline tests" [
      testCase "test_saxpy" $ execTest "saxpy.ll" saxpyTest saxpy
    , testCase "test_daxpy" $ execTest "daxpy.ll" daxpyTest daxpy
    , testCase "test_diag"  $ execTest "diag.ll" diagTest diag
    ]

  ]
