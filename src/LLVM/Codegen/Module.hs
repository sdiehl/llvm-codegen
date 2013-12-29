{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Codegen.Module (
  LLVM(..),
  runLLVM,
  emptyModule,

  addDefn,
  define,
  typedef,
  external,
) where

import Control.Applicative
import Control.Monad.State

import LLVM.General.AST
import LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST as AST


newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

typedef :: String -> Type -> LLVM ()
typedef name ty = addDefn $
  TypeDefinition (Name name) Nothing

global :: String -> Type -> C.Constant -> LLVM ()
global name ty val = addDefn $
  GlobalDefinition $ globalVariableDefaults {
    G.name        = Name name
  , G.type'       = ty
  , G.initializer = (Just val)
  }

external ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
external retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }
