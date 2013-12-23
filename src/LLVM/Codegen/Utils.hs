module LLVM.Codegen.Utils where

import Data.Bits
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)

bitsize :: Int
bitsize = bitSize (undefined :: Int)

ptrsize :: Int
ptrsize = sizeOf (undefined :: Ptr a)
