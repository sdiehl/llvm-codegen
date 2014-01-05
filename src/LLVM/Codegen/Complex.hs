{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LLVM.Codegen.Complex (
  complex,
  im,
  re,
  cadd,
  csub
) where

import Control.Applicative
import LLVM.Codegen.Types
import LLVM.Codegen.Instructions
import LLVM.Codegen.Constant
import LLVM.Codegen.Builder

import LLVM.General.AST (Type, Operand)

data Complex = Complex
  { complexType  :: Type
  , complexValue :: Operand
  } deriving (Eq, Show)

-- | Complex number type constructor
complex :: Type -> Type
complex ty = struct [ty, ty]

one :: Operand
one  = cons $ ci32 1

zero :: Operand
zero = cons $ ci32 0

-- | Extract the real part of a complex number
re :: Complex -> Codegen Operand
re x = gep (complexValue x) [zero, zero]

-- | Extract the imaginary part of a complex number
im :: Complex -> Codegen Operand
im x = gep (complexValue x) [zero, one]

-- | Lift elementwise operations for complex numbers
complexOp :: (Operand -> Operand -> Codegen Operand)-> Complex -> Complex -> Codegen Complex
complexOp op x y = do
  o <- alloca ty
  let oc = Complex ty o
  a <- liftA2 op (re x) (re y)
  b <- liftA2 op (im x) (im y)
  liftA2 store (re oc) a
  liftA2 store (im oc) b
  return $ oc
  where
    ty = complexType x

-- | Addition of complex numbers
cadd :: Complex -> Complex -> Codegen Complex
cadd = complexOp add

-- | Subtraction of complex numbers
csub :: Complex -> Complex -> Codegen Complex
csub = complexOp sub
