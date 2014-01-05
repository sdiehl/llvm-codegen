{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO

import Control.Applicative

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

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
import qualified LLVM.Codegen.Intrinsics as I

import LLVM.General.AST (Operand, Type)

-- saxpy [t] α x y out =
--  out <- α * x + y
--
-- α : Scalar t
-- x : Vector t
-- y : Vector t
axpy :: Type -> LLVM ()
axpy ty = do
  def "saxpy" i32 [
        (i32, "n")           -- element count
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

    for i (add one) (`lt` n) $ \ix -> do
      xi <- arrayGet xarr [ix]
      yi <- arrayGet yarr [ix]
      ax  <- fmul a xi
      sum <- fadd ax yi
      arraySet oarr [ix] sum

    return zero


saxpy = axpy f32
daxpy = axpy f64

callTest :: Stage
callTest (ctx, m, settings) = do
  x <- VM.replicate 64 (10 :: CDouble)
  y <- VM.replicate 64 (20 :: CDouble)
  o <- VM.replicate 64 (0 :: CDouble)
  xptr <- vectorArg x
  yptr <- vectorArg y
  optr <- vectorArg o
  let args = [argCInt 64, argCDouble 3, xptr, yptr, optr]

  -- Call the JIT'd function with the underlying pointers from the Haskell vectors.
  callAs ctx m "saxpy" retVoid args

  -- Print the output array that we wrote to from LLVM
  frozen <- V.freeze o
  let arr = V.toList frozen
  if arr == replicate 64 50 then
    return $ Right (ctx, m, settings)
  else
    return $ Left "output array does not match expected value"

compile :: LLVM a -> IO ()
compile m = do
  let ast = runLLVM (emptyModule "test module") m
  result <- runPipeline myPipeline defaultSettings ast
  case result of
    Left a -> print a
    Right b -> return ()

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

myPipeline :: Pipeline
myPipeline = [
    verifyPass
  , optimizePass 3
  , showPass
  , showAsmPass
  , callTest
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Pipeline tests"
  [
  testCase "test_daxpy" $ compile daxpy
  --, testCase "test_saxpy" $ compile saxpy
  ]
