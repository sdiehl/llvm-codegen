module LLVM.Codegen.Boxed (
  box,
  unbox
) where

import LLVM.Codegen.GC
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Builder

import LLVM.General.AST (Type, Operand)

-- | Store a boxed value in heap-allocated memory.
box :: Type -> Operand -> Codegen ()
box ty val = do
  size <- sizeof ty
  ptr <- gcmalloc [size]
  store val ptr

-- | Unbox a heap-allocated value as given type.
unbox :: Type -> Operand -> Codegen Operand
unbox ty ptr = do
  castptr <- bitcast (pointer ty) ptr
  load castptr
