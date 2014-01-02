module LLVM.Codegen.Constant (
  ci1, ci8, ci16, ci32, ci64,
  cf32, cf64,
  cfloat,
  cdouble,
  cnull,
  cundef,
  cstruct,
  cstructpack,
  carray,
  cvector,
  cstring,
  cstringz,

  Constant(..)
) where

import Data.Char

import LLVM.General.AST
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.Constant as C

ci1, ci8, ci16, ci32, ci64 :: (Integral a) => a -> C.Constant
ci1  = C.Int 1 . fromIntegral
ci8  = C.Int 8 . fromIntegral
ci16 = C.Int 16 . fromIntegral
ci32 = C.Int 32 . fromIntegral
ci64 = C.Int 64 . fromIntegral

cf32, cfloat :: Float -> C.Constant
cf32  = C.Float . F.Single
cfloat = cf32

cf64, cdouble :: Double -> C.Constant
cf64  = C.Float . F.Double
cdouble = cf64

cnull :: Type -> C.Constant
cnull ty = C.Null ty

cundef :: Type -> C.Constant
cundef = C.Undef

cstruct :: [C.Constant] -> C.Constant
cstruct values = C.Struct False values

cstructpack :: [C.Constant] -> C.Constant
cstructpack values = C.Struct True values

carray :: Type -> [C.Constant] -> C.Constant
carray ty values = C.Array ty values

cvector :: [C.Constant] -> C.Constant
cvector values = C.Vector values

-- | Null terminated constant string
cstringz :: String -> C.Constant
cstringz s = carray (IntegerType 8) chars
  where
    chars = map (ci8 . ord) s ++ [ci8 (0 :: Int)]

-- | Non-null terminated constant string
cstring :: String -> C.Constant
cstring s = carray (IntegerType 8) chars
  where
    chars = map (ci8 . ord) s

-- | Conversion between Haskell numeric values and LLVM constants
class Constant a where
  toConstant :: a -> C.Constant

instance Constant Bool where
  toConstant False = ci1 (0 :: Int)
  toConstant True  = ci1 (1 :: Int)

instance Constant Int where
  toConstant = ci64

instance Constant Float where
  toConstant = cf32

instance Constant Double where
  toConstant = cf64
