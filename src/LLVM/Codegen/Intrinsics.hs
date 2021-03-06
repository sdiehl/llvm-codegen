{-# LANGUAGE NoImplicitPrelude #-}

module LLVM.Codegen.Intrinsics where

import LLVM.General.AST
import LLVM.Codegen.Types
import LLVM.Codegen.Module

-------------------------------------------------------------------------------
-- Math Intrinsics
-------------------------------------------------------------------------------

-- | Math intrinsics

sin   = intrinsic f64 "llvm.sin.f64" [f64]
cos   = intrinsic f64 "llvm.cos.f64" [f64]
tan   = intrinsic f64 "llvm.tan.f64" [f64]
sqrt  = intrinsic f64 "llvm.sqrt.f64" [f64]
pow   = intrinsic f64 "llvm.pow.f64" [f64, f64]
exp   = intrinsic f64 "llvm.exp.f64" [f64]
log   = intrinsic f64 "llvm.log.f64" [f64]
log10 = intrinsic f64 "llvm.log10.f64" [f64]
fabs  = intrinsic f64 "llvm.fabs.f64" [f64]
floor = intrinsic f64 "llvm.floor.f64" [f64]
ceil  = intrinsic f64 "llvm.ceil.f64" [f64]
bswap = intrinsic i64 "llvm.bswap.i64" [i64]

-------------------------------------------------------------------------------
-- Memory Intrinsics
-------------------------------------------------------------------------------

-- | memcpy
memcpy :: Definition
memcpy = intrinsic f64 "llvm.memcpy" [f64]

-------------------------------------------------------------------------------
-- Lifetime Intrinsics
-------------------------------------------------------------------------------

-- | Lifetime analysis
lifestart, lifeend, invstart, invend :: Definition
lifestart = intrinsic void "llvm.lifetime.start" [i64, pointer i8]
lifeend   = intrinsic void "llvm.lifetime.end" [i64, pointer i8]
invstart  = intrinsic void "llvm.invariant.start" [i64, pointer i8]
invend    = intrinsic void "llvm.invariant.end" [pointer void, i64, pointer i8]

-------------------------------------------------------------------------------
-- NVPTX Intrinsics
-------------------------------------------------------------------------------

-- | threadIdx
tixx, tixy, tixz :: Definition
tixx = intrinsic i32 "llvm.nvvm.read.ptx.sreg.tid.x" []
tixy = intrinsic i32 "llvm.nvvm.read.ptx.sreg.tid.y" []
tixz = intrinsic i32 "llvm.nvvm.read.ptx.sreg.tid.z" []

-- | blockIdx
bixx, bixy, bixz :: Definition
bixx = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ctaid.x" []
bixy = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ctaid.y" []
bixz = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ctaid.z" []

-- | blockDim
bdimx, bdimy, bdimz :: Definition
bdimx = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ntid.x" []
bdimy = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ntid.y" []
bdimz = intrinsic i32 "llvm.nvvm.read.ptx.sreg.ntid.z" []

-- | gridDim
gdimx, gdimy, gdimz :: Definition
gdimx = intrinsic i32 "llvm.nvvm.read.ptx.sreg.nctaid.x" []
gdimy = intrinsic i32 "llvm.nvvm.read.ptx.sreg.nctaid.y" []
gdimz = intrinsic i32 "llvm.nvvm.read.ptx.sreg.nctaid.z" []

-- | syncthreads
syncthreads :: Definition
syncthreads = intrinsic void "llvm.cuda.syncthreads" []

-- | warpsize
warpsize :: Definition
warpsize = intrinsic void "llvm.nvvm.read.ptx.sreg.warpsize" []

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

noop :: Definition
noop = intrinsic void "llvm.donothing" []

prefetch :: Definition
prefetch = intrinsic void "llvm.prefetch" [pointer i8, i32, i32, i32]
