module LLVM.Codegen.Array (
  Order(..),
  Array(..),

  asArray,

  arrayArg,
  arrayPtr,
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
import qualified LLVM.General.AST.Constant as C

data Order = RowMajor
           | ColMajor deriving (Eq, Ord, Show)

data Array = Array
  { arrOrder   :: Order       -- ^ Array order
  , arrShape   :: [Operand]   -- ^ Array shape [i32]
  , arrStrides :: [Operand]   -- ^ Array strides
  -- , arrDim     :: Int         -- ^ Array dimensions
  , arrType    :: Type        -- ^ Array data type
  , arrData    :: Operand     -- ^ LLVM pointer to data
  } deriving (Eq, Ord, Show)

-- | Interpret a typed LLVM pointer as an Array
asArray :: Operand -> Type -> [Operand] -> Codegen Array
asArray arr ty shape =
    return $ Array RowMajor shape strides ty arr
  where
    strides = []

-- | Calculate the pointer for the C contigious array with given shape.
arrayPtr :: Array -> [Operand] -> Codegen Operand
arrayPtr arr ix = do
  steps <- forM [0..length sh] $ \i -> do
    let el = constant i32 1
    foldM mul el (drop (i+1) sh)
  let nullel = cons $ cnull i32
  pos <- foldM offset nullel (zip ix steps)
  dat <- load (arrData arr)
  gep dat [pos]
  where
    ty = arrType arr
    sh = arrShape arr
    offset p (x,y) = mul x y >>= add p

arraySet :: Array -> [Operand] -> Operand -> Codegen ()
arraySet arr ix val = do
  ptr <- arrayPtr arr ix
  store ptr val

arrayGet :: Array -> [Operand] -> Codegen Operand
arrayGet arr ix = do
  ptr <- arrayPtr arr ix
  load ptr

-- | Interpret an array ptr from a function as a local array.
arrayArg :: String -> Type -> [Operand] -> Codegen Array
arrayArg s ty size = do
  ptr <- getvar s
  asArray ptr ty size
