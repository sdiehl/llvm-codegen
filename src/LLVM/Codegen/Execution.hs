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

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

callAs :: Context -> Module -> String -> RetType a -> [Arg] -> IO a
callAs ctx m fname retty argtys =
  jit ctx $ \executionEngine ->
    EE.withModuleInEngine executionEngine m $ \ee -> do
      res <- EE.getFunction ee (AST.Name fname)
      case res of
        Nothing -> throwError (strMsg "No such function")
        Just fn -> callFFI fn retty argtys >>= return
