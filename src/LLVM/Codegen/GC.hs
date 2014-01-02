module LLVM.Codegen.GC (
  sizeOf,

  gcinit,
  gcmalloc,
  gccollect,
  gcdisable,
  gcdiagnostic,
) where

import LLVM.General.AST
import LLVM.Codegen.Types
import LLVM.Codegen.Utils
import LLVM.Codegen.Instructions
import LLVM.Codegen.Builder
import qualified LLVM.General.AST.Constant as C

-- XXX: make this emit an instruction
-- | sizeof instruction
sizeOf ::  Type -> Codegen Operand
sizeOf ty = return $ ConstantOperand $
  C.PtrToInt (C.GetElementPtr True nullty off) ptr
  where
    off    = [C.Int 32 1]
    nullty = C.Null $ pointer ty
    ptr    = IntegerType $ fromIntegral bitsize

gcinit :: Codegen Operand
gcinit = ccall "GC_init" []

gcmalloc :: [Operand] -> Codegen Operand
gcmalloc  = ccall "GC_malloc"

gccollect :: Codegen Operand
gccollect = ccall "GC_gcollect" []

gcdisable :: Codegen Operand
gcdisable = ccall "GC_disable" []

gcdiagnostic :: Codegen ()
gcdiagnostic = do
  heap  <- ccall "GC_get_heap_size" []
  free  <- ccall "GC_get_free_bytes" []
  total <- ccall "GC_get_free_bytes" []
  return ()
