{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.Codegen.Execution (
  withJit,
  vectorArg,
  callAs,
  jitCall,
  jitCall_,
  Lower(..)
) where

import Control.Monad.Except
import Control.Monad.Reader

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

-- | Convert a mutable vector into a libffi foreign pointer argument.
{-# NOINLINE vectorArg #-}
vectorArg :: Storable a => VM.MVector t a -> IO Arg
vectorArg v = return $ argPtr ptr
  where
    -- XXX: any way to use unsafeWithForeignPtr
    ptr = unsafeForeignPtrToPtr . fst $ VM.unsafeToForeignPtr0 v

-- | Call a JIT'd function as the specified type signature.
callAs :: Context -> Module -> String -> RetType a -> [Arg] -> IO a
callAs ctx m fname retty args =
  withJit ctx $ \executionEngine ->
    EE.withModuleInEngine executionEngine m $ \ee -> do
      mfn <- EE.getFunction ee (AST.Name fname)
      case mfn of
        Nothing -> throwError (strMsg $ "No such function: " ++ fname)
        Just fn -> callFFI fn retty args >>= return

-- | Call a JIT'd function within a ``Exec`` context.
jitCall fn retty args = do
  (ctx, llmod, _) <- ask
  liftIO $ callAs ctx llmod fn retty args

-- | Call a JIT'd function within a ``Exec`` context with no return value.
jitCall_ fn argtys = do
  (ctx, llmod, _) <- ask
  liftIO $ callAs ctx llmod fn retVoid argtys

-------------------------------------------------------------------------------
-- Type Conversion
-------------------------------------------------------------------------------

class Lower a where
  lower :: a -> IO Arg

instance Lower Int where
  lower = return . argCInt . fromIntegral

instance Lower Float where
  lower = return . argCFloat . realToFrac

instance Lower CInt where
  lower = return . argCInt

instance Lower CFloat where
  lower = return . argCFloat

instance Lower Double where
  lower = return . argCDouble . realToFrac

instance Storable a => Lower (VM.MVector t a) where
  lower = vectorArg

instance Storable a => Lower (V.Vector a) where
  lower v = V.thaw v >>= vectorArg
