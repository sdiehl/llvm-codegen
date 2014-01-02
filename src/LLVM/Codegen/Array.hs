module LLVM.Codegen.Array (
  Order(..),
  Array(..),

  arrayPtr,
  arraySet,
  arrayGet
) where

import LLVM.Codegen.Builder
import LLVM.Codegen.Instructions
import LLVM.General.AST (Name(..), Type, Operand)

data Order = RowMajor
           | ColMajor deriving (Eq, Ord, Show)

data Array = Array
  { arrOrder   :: Order       -- ^ Array order
  , arrShape   :: [Int]       -- ^ Array shape
  , arrStrides :: [Int]       -- ^ Array strides
  , arrDim     :: Int         -- ^ Number of dimensions
  , arrType    :: Type        -- ^ Array data type
  , arrData    :: Operand     -- ^ LLVM pointer to data
  } deriving (Eq, Ord, Show)

rowOffset :: Num a => a -> a -> a -> a
rowOffset row col ncols = row * ncols + col

colOffset :: Num a => a -> a -> a -> a
colOffset row col nrows = row + col * nrows

offset2D :: Array -> (Int, Int) -> Int
offset2D arr (a,b) = case arrOrder arr of
  RowMajor -> rowOffset a b ncols
  ColMajor -> colOffset a b nrows
  where
    shape = arrShape arr
    (ncols, nrows) = (shape !! 0, shape !! 1)

offsetND :: Array -> [Int] -> Int
offsetND arr idx = undefined

arrayPtr :: Array -> [Operand] -> Codegen Array
arrayPtr arr ixs = do
  undefined

arraySet :: Array -> [Operand] -> Operand -> Codegen Operand
arraySet arr ix val = do
  ptr <- arrayPtr arr ix
  store val (arrData ptr)

arrayGet :: Array -> [Operand] -> Codegen Operand
arrayGet arr ix = do
  ptr <- arrayPtr arr ix
  load (arrData ptr)
