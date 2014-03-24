{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

import LLVM.Codegen
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Build
import LLVM.Codegen.Execution
import LLVM.Codegen.Pipeline

import Foreign.LibFFI
import Foreign.C.Types

simple1 :: LLVM ()
simple1 =
  def "foo" i32 [(i32, "x")] $
    add a b
  where
     a = constant i32 100
     b = constant i32 201

simple2 :: LLVM ()
simple2 = do
  def "foo" i32 [(i32, "x")] $ do
    n <- arg "x"
    res <- call (fn "bar") [n]
    return res

  def "bar" i32 [(i32, "x")] $ do
    n <- arg "x"
    a <- mul n n
    return a

run :: Exec ()
run = do
  ret <- jitCall "foo" retCInt [argCInt 125]
  liftIO $ print ret
  return ()

main :: IO ()
main = do
  execSimple run simple1
  execSimple run simple2
  logOptSimple "simple.opt.ll" 2 simple2
  return ()
