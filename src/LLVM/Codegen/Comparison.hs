module LLVM.Codegen.Comparison (
  ult,
  ugt,
  ueq,
  une,
  ule,
  uge,

  slt,
  sgt,
  seq,
  sne,
  sle,
  sge,

  flt,
  fgt,
  feq,
  fne,
  fle,
  fge
) where

import Prelude hiding (seq)

import LLVM.Codegen.Builder
import LLVM.Codegen.Instructions
import LLVM.General.AST (Operand(..))
import qualified LLVM.General.AST.Constant as C
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

ult :: Operand -> Operand -> Codegen Operand
ult a b = icmp IP.ULT a b

ugt :: Operand -> Operand -> Codegen Operand
ugt a b = icmp IP.UGT a b

ueq :: Operand -> Operand -> Codegen Operand
ueq a b = icmp IP.EQ a b

une :: Operand -> Operand -> Codegen Operand
une a b = icmp IP.NE a b

ule :: Operand -> Operand -> Codegen Operand
ule a b = icmp IP.ULE a b

uge :: Operand -> Operand -> Codegen Operand
uge a b = icmp IP.UGE a b



slt :: Operand -> Operand -> Codegen Operand
slt a b = icmp IP.SLT a b

sgt :: Operand -> Operand -> Codegen Operand
sgt a b = icmp IP.SGT a b

seq :: Operand -> Operand -> Codegen Operand
seq a b = icmp IP.EQ a b

sne :: Operand -> Operand -> Codegen Operand
sne a b = icmp IP.NE a b

sle :: Operand -> Operand -> Codegen Operand
sle a b = icmp IP.SLE a b

sge :: Operand -> Operand -> Codegen Operand
sge a b = icmp IP.SGE a b


flt :: Operand -> Operand -> Codegen Operand
flt a b = fcmp FP.OLT a b

fgt :: Operand -> Operand -> Codegen Operand
fgt a b = fcmp FP.OGT a b

feq :: Operand -> Operand -> Codegen Operand
feq a b = fcmp FP.OEQ a b

fne :: Operand -> Operand -> Codegen Operand
fne a b = fcmp FP.ONE a b

fle :: Operand -> Operand -> Codegen Operand
fle a b = fcmp FP.OLE a b

fge :: Operand -> Operand -> Codegen Operand
fge a b = fcmp FP.UGE a b
