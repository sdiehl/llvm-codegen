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
  intrinsic,
  llintrinsic,
  printf,
) where

import Control.Applicative
import Control.Monad.State

import LLVM.General.AST
import LLVM.General.AST.Global as G
import LLVM.General.AST.AddrSpace
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

-- | Declare a toplevel opaque type in the current module.
opaquetypedef :: String -> LLVM ()
opaquetypedef nm = addDefn $
  TypeDefinition (Name nm) Nothing

-- | Declare a toplevel global constant in the current module.
globaldef :: String -> Type -> C.Constant -> LLVM Name
globaldef nm ty val = do
  addDefn $
    GlobalDefinition $ globalVariableDefaults {
      G.name        = Name nm
    , G.type'       = ty
    , G.initializer = (Just val)
    }
  return (Name nm)

-- | Declare a toplevel external function in the current module.
external :: Type -> String -> [(Type, Name)] -> LLVM Name
external retty label argtys = do
  addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = toParams argtys
    , returnType  = retty
    , basicBlocks = []
    }
  return (Name label)

-- | Declare an LLVM intrinsic.
intrinsic :: Type -> String -> [Type] -> Definition
intrinsic retty label argtys =
  GlobalDefinition $ functionDefaults {
       name        = Name label
     , parameters  = toUParams argtys
     , returnType  = retty
     , basicBlocks = []
     }

-- | Definition for C printf, hardcoded definition.
printf :: Definition
printf = GlobalDefinition $
  functionDefaults {
    name        = Name "printf"
  , parameters  =
     ([(Parameter (PointerType (IntegerType 8) (AddrSpace 0)) (UnName 0) [])], True)
  , returnType  = (IntegerType 32)
  , basicBlocks = []
  }

globalName :: Definition -> Name
globalName (GlobalDefinition (Function {name = x})) = x
globalName (GlobalDefinition (GlobalAlias {name = x})) = x
globalName (GlobalDefinition (GlobalVariable {name = x})) = x

-- | Produce a reference to an LLVM intrinsic.
llintrinsic :: Definition -> LLVM Name
llintrinsic intr = addDefn intr >> return (globalName intr)

toParams :: [(Type, Name)] -> ([Parameter], Bool)
toParams args = ([Parameter ty nm [] | (ty, nm) <- args], False)

toUParams :: [Type] -> ([Parameter], Bool)
toUParams args = ([Parameter ty (UnName nm) [] | (ty, nm) <- zip args [0..]], False)
