module LLVM.Codegen.Comparison (
  lt,
  gt,
  eq
) where

import LLVM.Codegen.Builder
import LLVM.Codegen.Instructions
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST (Operand(..))
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

{-
        unsigned  signed   floating
        --------- -------- --------
    lt  ULT       SLT      OLT
    gt  UGT       SGT      OGT
    le  ULE       SLE      OLE
    ge  UGE       SGE      OGE
    eq  EQ        EQ       OEQ
    ne  NE        NE       ONE
-}

intOperand (ConstantOperand (C.Int {})) = True
intOperand _ = False

floatOperand (ConstantOperand (C.Float {})) = True
floatOperand _ = False

lt :: Operand -> Operand -> Codegen Operand
lt x y = case (x,y) of
  (a,b) | intOperand a && intOperand b     -> icmp IP.ULT a b
  (a,b) | floatOperand a && floatOperand b -> fcmp FP.OLT a b

gt :: Operand -> Operand -> Codegen Operand
gt x y = case (x,y) of
  (a,b) | intOperand a && intOperand b     -> icmp IP.UGT a b
  (a,b) | floatOperand a && floatOperand b -> fcmp FP.OGT a b

eq :: Operand -> Operand -> Codegen Operand
eq x y = case (x,y) of
  (a,b) | intOperand a && intOperand b     -> icmp IP.EQ a b
  (a,b) | floatOperand a && floatOperand b -> fcmp FP.OEQ a b
