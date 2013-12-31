{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind#-}

module LLVM.Codegen.Logic where

import Control.Monad (forM )

import LLVM.Codegen.Builder
import LLVM.Codegen.Module
import LLVM.Codegen.Instructions

import LLVM.General.AST (Name(..), Type, Operand)

-------------------------------------------------------------------------------
-- Function
-------------------------------------------------------------------------------

-- | Construct a toplevel function.
def :: String -> Type -> [(Type, String)] -> Codegen Operand -> LLVM ()
def name retty argtys m = do
  define retty name argtys blocks
  where
    blocks = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM argtys $ \(ty, a) -> do
        avar <- alloca ty
        store avar (local (Name a))
        setvar a avar
      m >>= ret

-- | Construct a variable
var ty val name = undefined

-- | Construction
ifelse cond tr fl = undefined

while cond body = undefined

proj struct field = undefined
