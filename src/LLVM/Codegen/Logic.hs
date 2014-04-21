{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LLVM.Codegen.Logic (
  def,
  arg,
  var,
  avar,
  constant,
  ife,
  for,
  while,
  range,
  loopnest,
  proj,
  projass,
  seqn,

  imin,
  imax,

  debug,
  debugInt,
  debugFloat,

  true,
  false,

  one,
  zero
) where

import Control.Monad (forM, zipWithM_ )

import LLVM.Codegen.Builder
import LLVM.Codegen.Module
import LLVM.Codegen.Types
import LLVM.Codegen.Constant
import LLVM.Codegen.Instructions
import LLVM.Codegen.Structure

import qualified LLVM.General.AST.IntegerPredicate as IP

import qualified LLVM.General.AST.Global as G
import LLVM.General.AST (Name(..), Type, Operand, Definition(..))

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- | Constant false value
false :: Codegen Operand
false = return $ constant i1 0

-- | Constant true value
true :: Codegen Operand
true = return $ constant i1 1

-- | Constant integer zero
zero :: Operand
zero = constant i32 0

-- | Constant integer one
one :: Operand
one = constant i32 1

-------------------------------------------------------------------------------
-- Function
-------------------------------------------------------------------------------

-- | Construct a toplevel function.
def :: String -> Type -> [(Type, String)] -> Codegen Operand -> LLVM ()
def name retty argtys body = do
    mapM addDefn globals
    define retty name argtys blocks
  where
    (blocks, globals) = evalCodegen $ do
      entryBlock
      -- Map arguments into values in the symbol table
      forM argtys $ \(ty, a) -> do
        avar <- (a ++ ".addr") `named` alloca ty
        store avar (local (Name a))
        setvar a avar
      body >>= ret

-- | Retrieve the value of a locally named variable.
arg :: String -> Codegen Operand
arg s = getvar s >>= load

-- | Construct a named variable
var :: Type -> Operand -> String -> Codegen Operand
var ty val name = do
  ref <- alloca ty
  store ref val
  setvar name ref
  return ref

-- | Construct an unnamed variable
avar :: Type -> Operand -> Codegen Operand
avar ty val = do
  name <- freshName
  ref <- alloca ty
  store ref val
  return ref

constant ty val
  | ty == i1  = cons $ ci1  $ fromIntegral val
  | ty == i8  = cons $ ci8  $ fromIntegral val
  | ty == i16 = cons $ ci16 $ fromIntegral val
  | ty == i32 = cons $ ci32 $ fromIntegral val
  | ty == i64 = cons $ ci64 $ fromIntegral val
  | ty == f32 = cons $ cf32 $ fromIntegral val
  | ty == f64 = cons $ cf64 $ fromIntegral val
  | otherwise = error "Constant not supported"

-- | Construction a if/then/else statement
ife :: Type -> Operand -> Codegen Operand -> Codegen Operand -> Codegen Operand
ife ty cond tr fl = do
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
  phi ty [(trval, ifthen'), (flval, ifelse')]

-- | Construction a for statement
for :: Codegen Operand               -- ^ Iteration variable
    -> (Operand -> Codegen Operand)  -- ^ Iteration action
    -> (Operand -> Codegen Operand)  -- ^ Loop exit condition
    -> (Operand -> Codegen a)        -- ^ Loop body
    -> Codegen ()
for ivar inc cond body = do
  forcond <- addBlock "for.cond"
  forloop <- addBlock "for.loop"
  forinc  <- addBlock "for.inc"
  forexit <- addBlock "for.exit"

  i <- ivar
  br forcond

  setBlock forcond
  ival <- load i
  test <- cond ival
  cbr test forloop forexit

  setBlock forloop
  ival <- load i
  body ival
  br forinc

  setBlock forinc
  iinc <- inc ival
  store i iinc
  br forcond

  setBlock forexit
  return ()

-- | Construction for range statement
range :: String                 -- ^ Name of the iteration variable
      -> Codegen Operand        -- ^ Lower bound
      -> Codegen Operand        -- ^ Upper bound
      -> (Operand -> Codegen a) -- ^ Loop body
      -> Codegen ()
range ivar start stop body = do
  forcond <- addBlock "for.cond"
  forloop <- addBlock "for.loop"
  forinc  <- addBlock "for.inc"
  forexit <- addBlock "for.exit"

  lower <- start
  upper <- stop
  i <- var i32 lower ivar
  br forcond

  setBlock forcond
  ival <- load i
  test <- icmp IP.ULT ival upper
  cbr test forloop forexit

  setBlock forloop
  ival <- load i
  body ival
  br forinc

  setBlock forinc
  iinc <- add ival one
  store i iinc
  br forcond

  setBlock forexit
  return ()

-- | Construction a while statement
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

-- | Construct a multidimensional loop nest. The equivelant in C would be like the following:
--
-- @
-- int i, j;
-- for ( j = 0; j < 100; ++j ) {
--     for ( i = 0; i < 100; ++i ) {
--         .. loop body ..
--     }
-- }
-- @
loopnest :: [Int]                     -- ^ Start bounds
         -> [Int]                     -- ^ End bounds
         -> [Int]                     -- ^ loop steps
         -> ([Operand] -> Codegen a)  -- ^ Loop body
         -> Codegen ()
loopnest begins ends steps body = do
    -- Create the iteration variables
    ivars <- mapM makeIVar begins
    -- Build up the loops up recursively
    go ivars begins ends steps
  where
    makeIVar _ = avar i32 zero >>= load
    go ivars [] [] [] = (body ivars) >> return ()
    go ivars (b:bs) (e:es) (s:ss) = do
      let start = return $ constant i32 b
      let stop  = return $ constant i32 e
      range "i" start stop $ \_ ->
        go ivars bs es ss
    go _ _ _ _ = error "loop nest bounds are not equaly sized"

-- | Construction a record projection statement
proj :: Record -> Operand -> Name -> Codegen Operand
proj rty rec field =
  case idxOf field rty of
    Nothing -> error $ "No such field name: " ++ show field
    Just ix -> gep rec [constant i32 0, constant i32 ix]

-- | Construction a record assignment statement
projass :: Record -> Operand -> [Operand] -> Codegen ()
projass rty rec vals = do
  zipWithM_ ass (fieldsOf rty) (vals)
  return ()
  where
    ass fld val = do
      ptr <- proj rty rec fld
      store ptr val

-- | Construction of a sequence statement
seqn :: Codegen a -> Codegen b -> Codegen b
seqn = (>>)

-------------------------------------------------------------------------------
-- Comparison
-------------------------------------------------------------------------------

imin a b = do
  test <- icmp IP.ULT a b
  ife i32 test (return a) (return b)

imax a b = do
  test <- icmp IP.ULT a b
  ife i32 test (return a) (return b)

-------------------------------------------------------------------------------
-- Debug
-------------------------------------------------------------------------------

-- | Wrapper for debugging with printf
debug :: String -> [Operand] -> Codegen Operand
debug str vals = do
  addGlobal defn
  addGlobal printf

  fmt <- gep (global strnm) [zero, zero]
  call (fn "printf") (fmt : vals)
  where
    ty    = (array (len + 1) i8)
    cstr  = cstringz str
    len   = fromIntegral $ length str
    strnm = Name $ take 10 str
    defn  = GlobalDefinition $ G.globalVariableDefaults {
      G.name        = strnm
    , G.type'       = ty
    , G.initializer = (Just cstr)
    }

-- In C all float arguments to printf are automatically promoted to doubles.

debugInt :: String -> Operand -> Codegen Operand
debugInt fmt val = do
  cval <- zext i64 val
  debug fmt [cval]

debugFloat :: String -> Operand -> Codegen Operand
debugFloat fmt val = do
  cval <- fpext double val
  debug fmt [cval]
