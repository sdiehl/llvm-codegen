module LLVM.Codegen.Utils (
  bitsize,
  ptrsize,
  getCC,
  getLD
) where

import Data.Maybe
import Data.Bits

import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf)

import System.Environment
import Control.Applicative

bitsize :: Int
bitsize = bitSize (undefined :: Int)

ptrsize :: Int
ptrsize = sizeOf (undefined :: Ptr a)

getCC :: IO String
getCC = fromMaybe "gcc" <$> lookup "CC" <$> getEnvironment

getLD :: IO String
getLD = fromMaybe "ld" <$> lookup "LD" <$> getEnvironment
