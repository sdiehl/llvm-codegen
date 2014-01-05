module LLVM.Codegen.Comparison (
  ilt,
  igt,
  ieq,
  ine,
  ile,
  ige,

  flt,
  fgt,
  feq,
  fne,
  fle,
  fge
) where

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

-- XXX: add signed later, maybe

ilt :: Operand -> Operand -> Codegen Operand
ilt a b = icmp IP.ULT a b

igt :: Operand -> Operand -> Codegen Operand
igt a b = icmp IP.UGT a b

ieq :: Operand -> Operand -> Codegen Operand
ieq a b = icmp IP.EQ a b

ine :: Operand -> Operand -> Codegen Operand
ine a b = icmp IP.NE a b

ile :: Operand -> Operand -> Codegen Operand
ile a b = icmp IP.ULE a b

ige :: Operand -> Operand -> Codegen Operand
ige a b = icmp IP.UGE a b



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
