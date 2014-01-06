module LLVM.Codegen.Array (
  Order(..),
  Array(..),
  arrayType,

  asArray,

  arrayPtrC,
  arrayPtrF,
  arrayArg,
  arraySet,
  arrayGet
) where

import Control.Monad

import LLVM.Codegen.Builder
import LLVM.Codegen.Logic
import LLVM.Codegen.Types
import LLVM.Codegen.Constant
import LLVM.Codegen.Instructions
import LLVM.General.AST (Type, Operand)

data Order = RowMajor
           | ColMajor deriving (Eq, Ord, Show)

-- | Array representation. The equivelant C type would be:
--
-- @
-- typedef struct Array {
--    char *data;
--    intp *shape;
--    intp *strides;
-- } Array;
-- @
arrayType :: Type
arrayType = struct [
    pointer char   -- char *data
  , pointer intp   -- intp *shape
  , pointer intp   -- intp *strides
  ]

data Array = Array
  { arrOrder   :: Order       -- ^ Array order
  , arrShape   :: [Operand]   -- ^ Array shape
  , arrStrides :: [Operand]   -- ^ Array strides
  , arrDim     :: Int         -- ^ Array dimensions
  , arrType    :: Type        -- ^ Array data type
  , arrValue   :: Operand     -- ^ Underlying array struct
  } deriving (Eq, Ord, Show)

-- | Interpret a typed LLVM pointer as an Array
asArray :: Operand -> Type -> [Operand] -> Codegen Array
asArray arr ty shape =
    return $ Array RowMajor shape strides (length shape) ty arr
  where
    strides = []


-- | Generate instructions to calculate the pointer for the multidimensional C contiguous array with given
-- shape with for a given index.
--
-- @
-- Dim   : d
-- Shape : D = D_1 × D_2 ... × D_d
-- Index : I = (I_1, I_2, ... I_d)
--
-- offset(D, I) = \sum_{k=1}^d \left( \prod_{l=k+1}^d D_l \right) I_k
-- @

-- Offset instructions
offset :: Operand -> (Operand, Operand) -> Codegen Operand
offset p (x,y) = mul x y >>= add p

arrayPtrC :: Array -> [Operand] -> Codegen Operand
arrayPtrC arr ix = do
  steps <- forM [0..length sh] $ \i -> do
    foldM mul el (drop (i+1) sh)
  pos <- foldM offset nullel (zip ix steps)
  dat <- load (arrValue arr)
  gep dat [pos]
  where
    el     = constant i32 1
    nullel = cons $ cnull i32
    sh     = arrShape arr

-- | Generate instructions to calculate the pointer for the multidimensional Fotran contiguous array with
-- given shape with for a given index.
--
-- @
-- Dim   : d
-- Shape : D = D_0 × D_2 ... × D_d
-- Index : I = (I_1, I_2, ... I_d)
--
-- offset(D, I) = \sum_{k=1}^d \left( \prod_{\l=1}^{k-1} D_l \right) I_k
-- @

arrayPtrF :: Array -> [Operand] -> Codegen Operand
arrayPtrF arr ix = do
  steps <- forM [0..length sh] $ \i -> do
    foldM mul el (take i sh)
  pos <- foldM offset nullel (zip ix steps)
  dat <- load (arrValue arr)
  gep dat [pos]
  where
    el     = constant i32 1
    nullel = cons $ cnull i32
    sh     = arrShape arr

-- C contiguous by default
arrayPtr :: Array -> [Operand] -> Codegen Operand
arrayPtr = arrayPtrC

-- | Set an index of an array to a value
arraySet :: Array -> [Operand] -> Operand -> Codegen ()
arraySet arr ix val = do
  ptr <- arrayPtr arr ix
  store ptr val

-- | Index into an array retrieving a value.
arrayGet :: Array -> [Operand] -> Codegen Operand
arrayGet arr ix = do
  ptr <- arrayPtr arr ix
  load ptr

-- | Interpret an array ptr from a function as a local array.
arrayArg :: String -> Type -> [Operand] -> Codegen Array
arrayArg s ty size = do
  ptr <- getvar s
  asArray ptr ty size
