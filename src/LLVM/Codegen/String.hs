module LLVM.Codegen.String (
  fixedstr
) where

import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Constant (cstringz)

import LLVM.General.AST (Name(..))

-- | Construct a toplevel reference to an immutable null-terminated global string.
fixedstr :: [Char] -> LLVM Name
fixedstr str = globaldef strnm (array (len + 1) i8) (cstringz str)
  where
    strnm = take 10 str
    len = fromIntegral $ length str
