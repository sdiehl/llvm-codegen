module LLVM.Codegen.Execution where

import Control.Monad.Error

import Foreign.LibFFI
import System.Posix.DynamicLinker

import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST
import qualified LLVM.General.ExecutionEngine as EE

loadLib :: FilePath -> IO ()
loadLib lib = do
  dlopen lib [RTLD_NOW]
  return ()

-- JIT with sensible defaults
withJit :: Context -> (EE.MCJIT -> IO a) -> IO a
withJit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

-- | Call a JIT'd function as the specified type signature.
callAs :: Context -> Module -> String -> RetType a -> [Arg] -> IO a
callAs ctx m fname retty argtys =
  withJit ctx $ \executionEngine ->
    EE.withModuleInEngine executionEngine m $ \ee -> do
      mfn <- EE.getFunction ee (AST.Name fname)
      case mfn of
        Nothing -> throwError (strMsg $ "No such function: " ++ fname)
        Just fn -> callFFI fn retty argtys >>= return
