{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Codegen.Module (
  LLVM(..),
  runLLVM,
  emptyModule,

  addDefn,
  define,
  typedef,
  opaquetypedef,
  globaldef,
  external,
) where

import Control.Applicative
import Control.Monad.State

import LLVM.General.AST
import LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST as AST

-- | The LLVM builder monad.
newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

-- | Run the LLVM monad resulting in an AST.
runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

-- | The empty module.
emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

-- | Append a toplevel definition to the current module.
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- | Definition a toplevel function definition ni the current module.
define ::  Type -> String -> [(Type, String)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty (Name nm) [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

-- | Declare a toplevel type declaration in the current module.
typedef :: String -> Type -> LLVM ()
typedef nm ty = addDefn $
  TypeDefinition (Name nm) (Just ty)

opaquetypedef :: String -> LLVM ()
opaquetypedef nm = addDefn $
  TypeDefinition (Name nm) Nothing

-- | Declare a toplevel global constant in the current module.
globaldef :: String -> Type -> C.Constant -> LLVM ()
globaldef nm ty val = addDefn $
  GlobalDefinition $ globalVariableDefaults {
    G.name        = Name nm
  , G.type'       = ty
  , G.initializer = (Just val)
  }

-- | Declare a toplevel external function in the current module.
external ::  Type -> String -> [(Type, Name)] -> LLVM Name
external retty label argtys = do
  addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }
  return (Name label)
