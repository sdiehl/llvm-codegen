llvm-codegen
============

*llvm-codegen* is an abstraction layer on top of of llvm-general bindings. The goal is to provide a higher
level way to emit LLVM IR from Haskell for the purpose of writing numeric domain languages that compile to
native code for use against a C ABI or can be executed within the Haskell runtime via the LLVM JIT.

Usage
-----

The exposes a higher level buidler interface to constructing LLVM IR in an embedded DSL as well as a variety
of helper functions for mapping high-level constructs to LLVM IR and managing JIT execution.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LLVM.Codegen
import LLVM.Codegen.Build

import Foreign.LibFFI
import Control.Monad.Trans

program :: LLVM ()
program =
  def "foo" i32 [(i32, "x")] $
    add a b
  where
     a = constant i32 100
     b = constant i32 201

jit :: Exec ()
jit = do
  ret <- jitCall "foo" retCInt [argCInt 125]
  liftIO $ print ret
  return ()

main :: IO ()
main = do
  -- Run program inside the LLVM JIT
  execSimple jit program

  -- Log the LLVM IR to a file after running optimizer.
  logOptSimple "simple.opt.ll" 2 program

  return ()
```

With the following output:

```llvm
; ModuleID = 'simple module'

; Function Attrs: nounwind readnone
define i32 @foo(i32 %x) #0 {
entry:
  ret i32 301
}

attributes #0 = { nounwind readnone }
```

License
-------

Copyright 2013-2014 
Stephen Diehl ( stephen.m.diehl@gmail.com )

Released under the MIT License.
