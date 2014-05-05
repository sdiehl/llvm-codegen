llvm-codegen
============

*llvm-codegen* is an abstraction layer on top of of llvm-general bindings, or a compiler construction
framework. The goal is to provide a higher level way to emit LLVM IR from Haskell for the purpose of writing
numeric domain languages that compile to native code for use against a C ABI or can be executed within the
Haskell runtime via the LLVM JIT.

Code is still tentative, but contributions and bug reports are always welcome.

Install
-------

```bash
$ cabal configure
$ cabal install --only-dependencies
$ cabal build
```

For the test suite:

```bash
$ cabal configure --enable-tests
$ cabal test
```

For the documentation:

```bash
$ cabal haddock
```

Usage
-----

The exposes a higher level builder interface to constructing LLVM IR in an embedded DSL as well as a variety
of helper functions for mapping high-level constructs to LLVM IR and managing JIT execution.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LLVM.Codegen
import LLVM.Codegen.Build

import Foreign.LibFFI
import Control.Monad.Trans

program :: LLVM ()
program =
  def "foo" i32 [(i32, "x")] $ do
    let a = constant i32 100
    let b = constant i32 201
    add a b

jit :: Exec ()
jit = do
  ret <- jitCall "foo" retCInt [argCInt 125]
  liftIO $ print ret

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

Low Level Features
==================

Pipeline
--------

TODO

Execution
----------

TODO

LibFFI
------

Language Constructions
======================

Functions
---------

```cpp
int foo(int x) {
  int32_t a = 1;
  int64_t b = 1;
  return (a+b);
}
```

```haskell
testSimple :: LLVM ()
testSimple =
  def "foo" i32 [(i32, "x")] $
    add a b
  where
     a = constant i32 1
     b = constant i64 1000
```

```llvm
; ModuleID = 'simple module'

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}
```

The last statement in the Codegen monad implicitly returns the value as LLVM ``ret`` statement.

Printf
-----

Encoding the strings to write down ``printf`` from libc, is normally a pain so a ``debug`` function is
included to do this automatically.

```cpp
int main() {
  printf("%i", 42);
  return 0;
}
```

```haskell
testDebug :: LLVM ()
testDebug =
  def "main" i32 [] $
    debug "%i" [x]
  where
    x = constant i32 42
```

```llvm
; ModuleID = 'simple module'

@"%i" = global [3 x i8] c"%i\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @"%i", i32 0, i32 0), i32 42)
  ret i32 %0
}
```

Variables
---------

```cpp
uint8_t main(int x) {
  return (x < 1);
}
```

```haskell
  def "main" i1 [(i32, "x")] $ do
    x <- getvar "x"
    xv <- load x
```

```llvm
; ModuleID = 'simple module'

define i1 @main(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  %0 = load i32* %x.addr
  %1 = icmp ult i32 %0, 1
  ret i1 %1
}
```

The ``mem2reg`` optimizerz pass is always able to eliminate redudent stack allocations emitted using this
approach to local variables.

For Loops
---------

To generate code like the following C of nested loops:

```cpp
int i, j;
for ( j = 0; j < 100; ++j ) {
    for ( i = 0; i < 100; ++i ) {
        foo();
    }
}
```

We can use the `for` function to generate the appropriate loop blocks in our function.

```haskell
for :: Codegen Operand               -- ^ Iteration variable
    -> (Operand -> Codegen Operand)  -- ^ Iteration action
    -> (Operand -> Codegen Operand)  -- ^ Loop exit condition
    -> (Operand -> Codegen a)        -- ^ Loop body
    -> Codegen ()
```

```haskell
forloop :: LLVM ()
forloop = do
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
```

Which results in LLVM code like the following:

```llvm
; ModuleID = 'simple module'

declare i32 @foo()

define i32 @forloop() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32* %0
  br i1 true, label %for.loop, label %for.exit

for.loop:                                         ; preds = %for.cond
  %2 = load i32* %0
  %3 = alloca i32
  store i32 0, i32* %3
  br label %for.cond1

for.inc:                                          ; preds = %for.exit1
  %4 = add i32 1, %2
  store i32 %4, i32* %0
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  ret i32 0

for.cond1:                                        ; preds = %for.inc1, %for.loop
  %5 = load i32* %3
  br i1 true, label %for.loop1, label %for.exit1

for.loop1:                                        ; preds = %for.cond1
  %6 = load i32* %3
  %7 = call i32 @foo()
  br label %for.inc1

for.inc1:                                         ; preds = %for.loop1
  %8 = add i32 1, %6
  store i32 %8, i32* %3
  br label %for.cond1

for.exit1:                                        ; preds = %for.cond1
  br label %for.inc
}
```

Tuples
------

TODO

Records
-------

Structures in LLVM are just packed/unpacked structures which refer to fields by numerical index. We build on
top of this to interface to wrap the records in a structure which allows us to refer to names by human
readable strings which map to indicies which perform the relevant GetElementPtr operations on the underlying
struct.

```cpp
struct myrecord {
  int kirk;
  float spock;
};
```

```haskell
testRecord :: LLVM ()
testRecord = do
  rec <- record "myrecord" [("kirk", i32), ("spock", f32)]
  def "main" i32 [] $ do
    x <- allocaRecord rec
    xp <- proj rec x "kirk"
    load xp
```

```llvm
; ModuleID = 'simple module'

define i32 @main() {
entry:
  %0 = alloca { i32, float }
  %1 = getelementptr inbounds { i32, float }* %0, i32 0, i32 0
  %2 = load i32* %1
  ret i32 %2
}
```

Arrays and Matrices
-------------------

TODO

License
=======

Copyright 2013-2014 
Stephen Diehl ( stephen.m.diehl@gmail.com )

Released under the MIT License.
