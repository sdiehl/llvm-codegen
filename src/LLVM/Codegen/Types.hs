module LLVM.Codegen.Types (
  i1, i8, i16, i32, i64, i128,
  f32, f64,
  intp,
  intp_ptr,
  void,
  pointer,
  array,
  vector,
  struct,
  fntype,

  -- aliases
  float,
  double,
  char,

  isFloat,
  isInt
) where


import Data.Word
import LLVM.Codegen.Utils
import LLVM.General.AST hiding (vector)

import LLVM.General.AST.AddrSpace

-- | Integer types
i1, i8, i16, i32, i64, i128 :: Type
i1   = IntegerType 1
i8   = IntegerType 8
i16  = IntegerType 16
i32  = IntegerType 32
i64  = IntegerType 64
i128 = IntegerType 128

intp, intp_ptr :: Type
intp = IntegerType (8 * fromIntegral ptrsize)
intp_ptr = PointerType intp (AddrSpace 0)

-- | Floating point types
f32, f64 :: Type
f32  = FloatingPointType 32 IEEE
f64  = FloatingPointType 64 IEEE

float, double, char :: Type
float = f32
double = f64
char = i8

-- | Void type
void :: Type
void = VoidType

-- | Pointer type constructor
pointer :: Type -> Type
pointer ty = PointerType ty (AddrSpace 0)

-- | Array type constructor
array :: Word64 -> Type -> Type
array elements ty = ArrayType elements ty

-- | Vector type constructor
vector :: Word32 -> Type -> Type
vector width ty = VectorType width ty

-- | Struct type constructor
struct :: [Type] -> Type
struct fields = StructureType True fields

-- | Function type constructor
fntype :: Type -> [Type] -> Type
fntype argtys retty = FunctionType argtys retty True

isFloat :: Type -> Bool
isFloat (FloatingPointType _ _) = True
isFloat _ = False

isInt :: Type -> Bool
isInt (IntegerType _) = True
isInt _ = False
