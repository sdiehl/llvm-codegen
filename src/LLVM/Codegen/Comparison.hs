module LLVM.Codegen.Comparison (
  lt,
  gt,
  eq,
  ne,
  le,
  ge
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

intOperand :: Operand -> Bool
intOperand (ConstantOperand (C.Int {})) = True
intOperand _ = False

floatOperand :: Operand -> Bool
floatOperand (ConstantOperand (C.Float {})) = True
floatOperand _ = False

lt :: Operand -> Operand -> Codegen Operand
lt x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.ULT a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.OLT a b
  (a,b)                                    -> icmp IP.ULT a b
  {-_ -> error "Trying to compare non-arithmetic values"-}

gt :: Operand -> Operand -> Codegen Operand
gt x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.UGT a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.OGT a b
  _ -> error "Trying to compare non-arithmetic values"

eq :: Operand -> Operand -> Codegen Operand
eq x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.EQ a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.OEQ a b
  _ -> error "Trying to compare non-arithmetic values"

ne :: Operand -> Operand -> Codegen Operand
ne x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.NE a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.ONE a b
  _ -> error "Trying to compare non-arithmetic values"

le :: Operand -> Operand -> Codegen Operand
le x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.ULE a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.OLE a b
  _ -> error "Trying to compare non-arithmetic values"


ge :: Operand -> Operand -> Codegen Operand
ge x y = case (x,y) of
  (a,b) | intOperand a   || intOperand b   -> icmp IP.UGE a b
  (a,b) | floatOperand a || floatOperand b -> fcmp FP.OGE a b
  _ -> error "Trying to compare non-arithmetic values"
