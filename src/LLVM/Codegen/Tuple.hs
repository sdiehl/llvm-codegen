module LLVM.Codegen.Tuple (
  inl,
  inr,
  tupleType,
  mkPair
) where

import LLVM.Codegen.Builder
import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Constant
import LLVM.Codegen.Instructions
import LLVM.Codegen.Structure

import qualified LLVM.General.AST.IntegerPredicate as IP

import qualified LLVM.General.AST.Global as G
import LLVM.General.AST (Name(..), Type, Operand, Definition(..))

inl, inr :: Operand -> Codegen Operand
inl x = load =<< inlPtr x
inr x = load =<< inrPtr x

inlPtr, inrPtr :: Operand -> Codegen Operand
inlPtr x = gep x [ci 0, ci 0]
inrPtr x = gep x [ci 0, ci 1]

ci :: Integral a => a -> Operand
ci = cons . ci32 . fromIntegral

tupleType :: Type -> Type -> Type
tupleType a b = struct [ a , b ]

mkPair :: Type -> Type -> Operand -> Operand -> Codegen Operand
mkPair aty bty a b = do
  pair <- alloca $ tupleType aty bty
  l <- inlPtr pair
  r <- inrPtr pair
  store l a
  store r b
  return pair
