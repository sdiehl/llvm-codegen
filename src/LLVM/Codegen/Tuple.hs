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
inl x = inlPtr x >>= load
inr x = inrPtr x >>= load

inlPtr, inrPtr :: Operand -> Codegen Operand
inlPtr x = gep x [foo 0, foo 0]
inrPtr x = gep x [foo 0, foo 1]

foo = cons . ci32 . fromIntegral

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
