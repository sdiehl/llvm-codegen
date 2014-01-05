{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM.Codegen.Builder (
  instr,
  terminator,
  named,
  addBlock,
  setBlock,
  entryBlock,
  createBlocks,
  createGlobals,

  getBlock,
  getTerm,
  addGlobal,

  local,
  global,
  fn,

  freshName,
  getvar,
  setvar,

  Codegen,
  evalCodegen,
) where

import LLVM.Codegen.NameSupply

import Data.Word
import Data.List
import Data.Function
import Data.String

import Control.Monad.State
import Control.Applicative

import qualified Data.Map as Map

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- ^ Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- ^ Blocks for function
  , symtab       :: SymbolTable              -- ^ Function scope symbol table
  , blockCount   :: Int                      -- ^ Count of basic blocks
  , count        :: Word                     -- ^ Count of unnamed instructions
  , names        :: Names                    -- ^ Name Supply
  , globals      :: [Definition]             -- ^ Globals to add to outer module
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- ^ Block index
  , stack :: [Named Instruction]            -- ^ Stack of instructions
  , term  :: Maybe (Named Terminator)       -- ^ Block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

-- | Evaluate the result Codegen monad into a BasicBlock AST.
createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

createGlobals :: CodegenState -> [Definition]
createGlobals m = globals m

evalCodegen :: Codegen a -> ([BasicBlock], [Definition])
evalCodegen m = (createBlocks cs, createGlobals cs)
  where cs = execCodegen [] m

-- | Evaluate the Codegen monad returning the state accrued.
execCodegen :: [(String, Operand)] -> Codegen a -> CodegenState
execCodegen vars m = execState (runCodegen m) emptyCodegen { symtab = vars }

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

-- | Evaluate the result Codegen monad into a BasicBlock AST.
defaultEntry :: String
defaultEntry = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name defaultEntry) Map.empty [] 1 0 Map.empty []

entryBlock :: Codegen Name
entryBlock = do
  entry <- addBlock defaultEntry
  setBlock entry
  return entry

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

freshName :: Codegen Name
freshName = UnName <$> fresh

-- | Return the current basic block
current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-- | Append an instruction to current basic block
instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

-- | Set the terminator to current basic block
terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-- | Explictly set the llvm variable name /%foo/ instead of using a default unnamed variable.
named :: String -> Codegen a -> Codegen Operand
named iname m = m >> do
  blk <- current
  let b = Name iname
      (_ := x) = last (stack blk)
  modifyBlock $ blk { stack = init (stack blk) ++ [b := x] }
  return $ local b

instance IsString Name where
  fromString = Name . fromString

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

-- | Set a named value in the symbol table.
setvar :: String -> Operand -> Codegen ()
setvar var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

-- | Retrieve a named value from the symbol table.
getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------

-- | Reference to a local value
local :: Name -> Operand
local = LocalReference

-- | Reference to a local value
global :: Name -> Operand
global = ConstantOperand . C.GlobalReference

-- | Reference to a function
fn :: Name -> Operand
fn = ConstantOperand . C.GlobalReference

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

-- | Locally add a global to be added when the function is attached to a module.
addGlobal :: Definition -> Codegen ()
addGlobal def = do
  glb <- gets globals
  modify $ \s -> s { globals = def : glb }

-- | Append a new named basic block to a function
addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

-- | Set the current basic block
setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

getTerm :: Codegen (Maybe (Named Terminator))
getTerm = do
  blk <- current
  return (term blk)

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }
