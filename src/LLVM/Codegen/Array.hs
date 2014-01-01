module LLVM.Codegen.Array (
  Order(..),
  Array(..)
) where

import LLVM.General.AST (Name(..), Type)

data Order = RowMajor | ColMajor deriving (Eq, Ord, Show)

data Array = Array
    { arrOrder  :: Order
    , arrSize   :: (Int, Int)
    , arrDim    :: Int
    , arrType   :: Type
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
    (ncols, nrows) = arrSize arr

offsetND :: Array -> [Int] -> Int
offsetND arr idx = undefined
offsetND arr idx = undefined
