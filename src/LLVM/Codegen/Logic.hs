{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LLVM.Codegen.Logic (
  def,
  var,
  constant,
  ife,
  for,
  while,
  range,
  proj,
  caseof,
  seqn,
  fixedstr,

  true,
  false,

  one,
  zero
) where

import Control.Monad (forM )

import LLVM.Codegen.Builder
import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Constant
import LLVM.Codegen.Instructions
import LLVM.Codegen.Structure

import qualified LLVM.General.AST.IntegerPredicate as IP

import LLVM.General.AST (Name(..), Type, Operand)

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

false :: Codegen Operand
false = return $ constant i1 0

true :: Codegen Operand
true = return $ constant i1 1

zero :: Operand
zero = constant i32 0

one :: Operand
one = constant i32 1

-------------------------------------------------------------------------------
-- Function
-------------------------------------------------------------------------------

-- | Construct a toplevel function.
def :: String -> Type -> [(Type, String)] -> Codegen Operand -> LLVM ()
def name retty argtys m = do
  define retty name argtys blocks
  where
    blocks = createBlocks $ execCodegen [] $ do
      entryBlock
      -- Map arguments into values in the symbol table
      forM argtys $ \(ty, a) -> do
        avar <- alloca ty
        store avar (local (Name a))
        setvar a avar
      m >>= ret

-- | Construct a named variable
var :: Type -> Operand -> String -> Codegen Operand
var ty val name = do
  ref <- alloca ty
  store ref val
  setvar name ref
  return ref

{-constant :: Type -> (forall a. Num a => a) -> Operand-}
constant ty val
  | ty == i1  = cons $ ci1  $ fromIntegral val
  | ty == i8  = cons $ ci8  $ fromIntegral val
  | ty == i16 = cons $ ci16 $ fromIntegral val
  | ty == i32 = cons $ ci32 $ fromIntegral val
  | ty == i64 = cons $ ci64 $ fromIntegral val
  | ty == f32 = cons $ cf32 $ fromIntegral val
  | ty == f64 = cons $ cf64 $ fromIntegral val

-- | Construction a if/then/else statement
ife :: Operand -> Codegen Operand -> Codegen Operand -> Codegen Operand
ife cond tr fl = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cbr cond ifthen ifelse

  setBlock ifthen
  trval <- tr
  br ifexit
  ifthen' <- getBlock

  setBlock ifelse
  flval <- fl
  br ifexit
  ifelse' <- getBlock

  setBlock ifexit
  phi double [(trval, ifthen'), (flval, ifelse')]

-- | Construction a for statement
for :: Codegen Operand               -- ^ Iteration variable
    -> Codegen Operand               -- ^ Action to take on each loop iteration ( increment )
    -> (Operand -> Codegen Operand)  -- ^ Loop exit condition
    -> (Operand -> Codegen a)        -- ^ Loop body
    -> Codegen ()
for ivar inc cond body = do
  forcond <- addBlock "for.cond"
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  i <- ivar
  n <- inc
  br forcond

  setBlock forcond
  ival <- load i
  test <- cond ival
  cbr test forloop forexit

  setBlock forloop
  body i
  ival <- load i
  iinc <- add ival n
  store i iinc
  br forcond

  setBlock forexit
  return ()

-- | Construction for range statement
range :: String             -- ^ Name of the iteration variable
      -> Codegen Operand    -- ^ Lower bound
      -> Codegen Operand    -- ^ Upper bound
      -> Codegen a          -- ^ Loop body
      -> Codegen ()
range ivar start stop body = do
  forcond <- addBlock "for.cond"
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  lower <- start
  upper <- stop
  i <- var i32 lower ivar
  br forcond

  setBlock forcond
  test <- icmp IP.SLT i upper
  cbr test forloop forexit

  setBlock forloop
  body
  ival <- load i
  iinc <- add ival one
  store i iinc
  br forcond

  setBlock forexit
  return ()

while :: Codegen Operand -> Codegen a -> Codegen ()
while cond body = do
  forcond <- addBlock "while.cond"
  forloop <- addBlock "while.loop"
  forexit <- addBlock "while.exit"

  br forcond

  setBlock forcond
  test <- cond
  cbr test forloop forexit

  setBlock forloop
  body
  br forcond

  setBlock forexit
  return ()

-- | Construction a record projection statement
proj :: RecordType -> Operand -> Name -> Codegen Operand
proj rty rec field =
  case idxOf field rty of
    Nothing -> error $ "No such field name: " ++ show field
    Just ix -> do
      gep rec [constant i32 0, constant i32 ix]

-- | Construction a case statement
caseof val brs = undefined

-- | Construction of a sequence statement
seqn :: Codegen a -> Codegen b -> Codegen b
seqn a b = a >> b

-- | Construct a toplevel reference to an immutable null-terminated global string.
fixedstr :: [Char] -> LLVM Name
fixedstr str = globaldef ".str" (array (len + 1) i8) (cstringz str)
  where
    len = fromIntegral $ length str
