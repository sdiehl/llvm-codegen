module LLVM.Codegen.Instructions (
 add,
 alloca,
 and,
 ashr,
 atomicrmw,
 bitcast,
 br,
 call,
 cbr,
 ccall,
 cmpxchg,
 cons,
 extractelement,
 extractvalue,
 fadd,
 fcmp,
 fdiv,
 fence,
 fmul,
 fpext,
 fptosi,
 fptoui,
 fptrunc,
 fsub,
 gep,
 icmp,
 insertelement,
 insertvalue,
 inttoptr,
 load,
 lshr,
 mul,
 or,
 phi,
 ptrtoint,
 ref,
 ret,
 ret_,
 sdiv,
 sext,
 shl,
 shufflevector,
 singlethreaded,
 sitofp,
 srem,
 store,
 sub,
 switch,
 toArgs,
 trunc,
 udiv,
 uitofp,
 unreachable,
 urem,
 xor,
 zext
) where

import Prelude hiding (and, or)

import Data.Word
import LLVM.General.AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
{-import qualified LLVM.General.AST.FloatingPointPredicate as FP-}
import LLVM.General.AST.RMWOperation (RMWOperation)

import LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.General.AST.IntegerPredicate (IntegerPredicate)

import LLVM.Codegen.Builder (instr, terminator, Codegen)

-------------------------------------------------------------------------------
-- Logical
-------------------------------------------------------------------------------

-- | Logical and
and ::  Operand -> Operand -> Codegen Operand
and a b = instr $ And a b []

-- | Logical or
or ::  Operand -> Operand -> Codegen Operand
or a b = instr $ Or a b []

-- | Logical xor
xor ::  Operand -> Operand -> Codegen Operand
xor a b = instr $ Xor a b []

-------------------------------------------------------------------------------
-- Integer Arithmetic
-------------------------------------------------------------------------------

-- | Integer addition
add :: Operand -> Operand -> Codegen Operand
add a b = instr $ Add False False a b []

-- | Integer subtraction
sub :: Operand -> Operand -> Codegen Operand
sub a b = instr $ Sub False False a b []

-- | Integer multiplication
mul :: Operand -> Operand -> Codegen Operand
mul a b = instr $ Mul False False a b []

-- | Unsigned integer division
udiv :: Operand -> Operand -> Codegen Operand
udiv a b = instr $ UDiv False a b []

-- | Signed integer division
sdiv :: Operand -> Operand -> Codegen Operand
sdiv a b = instr $ SDiv False a b []

-- | Unsigned integer division
urem :: Operand -> Operand -> Codegen Operand
urem a b = instr $ URem a b []

-- | Signed integer division
srem :: Operand -> Operand -> Codegen Operand
srem a b = instr $ SRem a b []

shl ::  Operand -> Operand -> Codegen Operand
shl a b = instr $ Shl False False a b []

lshr ::  Operand -> Operand -> Codegen Operand
lshr a b = instr  $LShr False a b []

ashr ::  Operand -> Operand -> Codegen Operand
ashr a b = instr $ AShr False a b []

-------------------------------------------------------------------------------
-- Floating Point Arithmetic
-------------------------------------------------------------------------------

-- | Floating point addition
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd a b []

-- | Floating point subtraction
fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub a b []

-- | Floating point multiplication
fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul a b []

-- | Floating point division
fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv a b []

-------------------------------------------------------------------------------
-- Comparison
-------------------------------------------------------------------------------

-- | Floating point comparison
fcmp :: FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

-- | Integer comparison
icmp :: IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

cons :: C.Constant -> Operand
cons = ConstantOperand

-------------------------------------------------------------------------------
-- Casts
-------------------------------------------------------------------------------

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

trunc :: Type -> Operand -> Codegen Operand
trunc ty a = instr $ Trunc a ty []

zext :: Type -> Operand -> Codegen Operand
zext ty a = instr $ ZExt a ty []

sext :: Type -> Operand -> Codegen Operand
sext ty a = instr $ SExt a ty []

fptoui :: Type -> Operand -> Codegen Operand
fptoui ty a = instr $ FPToUI a ty []

fptosi :: Type -> Operand -> Codegen Operand
fptosi ty a = instr $ FPToSI a ty []

sitofp:: Type -> Operand -> Codegen Operand
sitofp ty a = instr $ SIToFP a ty []

fptrunc :: Type -> Operand -> Codegen Operand
fptrunc ty a = instr $ FPTrunc a ty []

fpext :: Type -> Operand -> Codegen Operand
fpext ty a = instr $ FPExt a ty []

ptrtoint :: Type -> Operand -> Codegen Operand
ptrtoint ty a = instr $ PtrToInt a ty []

inttoptr :: Type -> Operand -> Codegen Operand
inttoptr ty a  = instr $ IntToPtr a ty []

bitcast :: Type -> Operand -> Codegen Operand
bitcast ty a = instr $ BitCast a ty []

-------------------------------------------------------------------------------
-- Function Calls
-------------------------------------------------------------------------------

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- | Call an external C function.
ccall :: String -> [Operand] -> Codegen Operand
ccall name args = instr $ Call
  { isTailCall = False
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right fn
  , arguments = toArgs args
  , functionAttributes = []
  , metadata = []
  }
  where
    fn = ConstantOperand (C.GlobalReference (Name name))

-------------------------------------------------------------------------------
-- Memory Access
-------------------------------------------------------------------------------

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = (instr $ Store False ptr val Nothing 0 []) >> return ()

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

gep ::  Operand -> [Operand] -> Codegen Operand
gep val idx = instr $ GetElementPtr True val idx []

-- | Return a reference to the given value, like (&) in C.
ref :: Operand -> Codegen Operand
ref val = instr $ GetElementPtr True val [cons $ C.Int 32 0] []

-------------------------------------------------------------------------------
-- Control Flow
-------------------------------------------------------------------------------

-- | Branch
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

-- | Conditional branch
cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- | Phi node
phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

-- | Return value
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- | Return void
ret_ :: Codegen (Named Terminator)
ret_ = terminator $ Do $ Ret Nothing []

switch :: Operand -> Name -> [(C.Constant, Name)] -> Codegen (Named Terminator)
switch val def pats = terminator $ Do $ Switch val def pats []

unreachable :: Codegen (Named Terminator)
unreachable = terminator $ Do $ Unreachable []

---------------------------------------------------------------------------------
-- Vector Operations
-------------------------------------------------------------------------------

extractelement :: Operand -> Operand -> Codegen Operand
extractelement vec idx = instr $ ExtractElement vec idx []

insertelement :: Operand -> Operand -> Operand -> Codegen Operand
insertelement vec el idx = instr $ InsertElement vec el idx []

shufflevector :: Operand -> Operand -> C.Constant -> Codegen Operand
shufflevector a b msk = instr $ ShuffleVector a b msk []

extractvalue :: Operand -> [Word32] -> Codegen Operand
extractvalue a idx = instr $ ExtractValue a idx []

insertvalue :: Operand -> Operand -> [Word32] -> Codegen Operand
insertvalue val elt idx = instr $ InsertValue val elt idx []

-------------------------------------------------------------------------------
-- Atomic Operations
-------------------------------------------------------------------------------

singlethreaded :: Atomicity
singlethreaded = Atomicity True SequentiallyConsistent

fence :: Codegen Operand
fence = instr $ Fence singlethreaded []

cmpxchg :: Operand -> Operand -> Operand -> Codegen Operand
cmpxchg ptr val rep = instr $ CmpXchg False ptr val rep singlethreaded []

atomicrmw :: RMWOperation -> Operand -> Operand -> Codegen Operand
atomicrmw op ptr val = instr $ AtomicRMW False op ptr val singlethreaded []
