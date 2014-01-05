module LLVM.Codegen.Execution where

import Control.Monad.Error

import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable
import Foreign.C.Types

import Foreign.LibFFI
import System.Posix.DynamicLinker

import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST
import qualified LLVM.General.ExecutionEngine as EE

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- | Dynamically load the given library into the current process of the JIT.
loadLib :: FilePath -> IO ()
loadLib lib = do
  dlopen lib [RTLD_LAZY]
  return ()

-- JIT with sensible defaults
withJit :: Context -> (EE.MCJIT -> IO a) -> IO a
withJit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

{-# NOINLINE withVectorPtrArg #-}
withVectorPtrArg :: Storable a => VM.MVector t a -> (Arg -> IO b) -> IO b
withVectorPtrArg v m = m (argPtr ptr)
  where
    ptr = unsafeForeignPtrToPtr . fst $ VM.unsafeToForeignPtr0 v

{-# NOINLINE withVectorPtr #-}
withVectorPtr :: Storable a => VM.MVector t a -> (Ptr a -> IO b) -> IO b
withVectorPtr v m = m ptr
  where
    ptr = unsafeForeignPtrToPtr . fst $ VM.unsafeToForeignPtr0 v

{-# NOINLINE vectorArg #-}
vectorArg :: Storable a => VM.MVector t a -> Arg
vectorArg v = argPtr ptr
  where
    ptr = unsafeForeignPtrToPtr . fst $ VM.unsafeToForeignPtr0 v

runArray :: FunPtr a -> IO ()
runArray fn = do
  v <- VM.replicate 64 (32 :: CInt)
  withVectorPtrArg v $ \arr -> do
    callFFI fn retVoid [arr]
    frozen <- V.freeze v
    print $ V.toList frozen
  return ()

-- | Call a JIT'd function as the specified type signature.
callAs :: Context -> Module -> String -> RetType a -> [Arg] -> IO a
callAs ctx m fname retty args =
  withJit ctx $ \executionEngine ->
    EE.withModuleInEngine executionEngine m $ \ee -> do
      mfn <- EE.getFunction ee (AST.Name fname)
      case mfn of
        Nothing -> throwError (strMsg $ "No such function: " ++ fname)
        Just fn -> callFFI fn retty args >>= return
