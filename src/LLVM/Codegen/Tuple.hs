module LLVM.Codegen.Tuple (
  Pair,
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

inl, inr :: Pair -> Codegen Operand
inl x = inlPtr (pairValue x) >>= load
inr x = inrPtr (pairValue x) >>= load

inlPtr, inrPtr :: Operand -> Codegen Operand
inlPtr x = gep x [ci 0, ci 0]
inrPtr x = gep x [ci 0, ci 1]

ci :: Integral a => a -> Operand
ci = cons . ci32 . fromIntegral

tupleType :: Type -> Type -> Type
tupleType a b = struct [ a , b ]

data Pair = Pair
  { pairValue     :: Operand
  , pairLeftType  :: Type
  , pairRightType :: Type
  } deriving (Eq, Ord, Show)

mkPair :: Type -> Type
       -> Operand -> Operand
       -> Codegen Pair
mkPair aty bty a b = do
  pair <- alloca $ tupleType aty bty
  l <- inlPtr pair
  r <- inrPtr pair
  store l a
  store r b
  return $ Pair pair aty bty
