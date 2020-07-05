{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Hapstone.Internal.Arm
Description : RISC-V architecture header ported using C2HS + some boilerplate
Copyright   : (c) Inokentiy Babushkin, Gregory T. Sullivan 2020
License     : BSD3
Maintainer  : Inokentiy Babushkin <inokentiy.babushkin@googlemail.com>
Stability   : experimental

This module contains RISC-V specific datatypes and their respective Storable
instances. Most of the types are used internally and can be looked up here.
Some of them are currently unused, as the headers only define them as symbolic
constants whose type is never used explicitly, which poses a problem for a
memory-safe port to the Haskell language, this is about to get fixed in a
future version.

Apart from that, because the module is generated using C2HS, some of the
documentation is misplaced or rendered incorrectly, so if in doubt, read the
source file.
-}
module Hapstone.Internal.RISCV where

#include <capstone/riscv.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

-- | RISC-V Operand type for instruction's operands
-- [ RiscvOpInvalid | RiscvOpReg | RiscvOpImm | RiscvOpMem ]
{#enum riscv_op_type as RiscvOpType {underscoreToCase} deriving (Show, Eq, Bounded)#}

-- | RISC-V Instruction's operand referring to memory
--   This is associated with RISCV_OP_MEM operand type above
data RiscvOpMemRef = RiscvOpMemRef -- struct riscv_op_mem
  { base :: Word32                 -- base register
  , disp :: Int64                  -- displacement/offset vaue
  } deriving (Show, Eq)

instance Storable RiscvOpMemRef where
  sizeOf _ = {#sizeof cs_riscv_op#}
  alignment _ = {#alignof cs_riscv_op#}
  peek p = RiscvOpMemRef
    <$> (fromIntegral <$> {#get riscv_op_mem->base#} p) -- base
    <*> (fromIntegral <$> {#get riscv_op_mem->disp#} p) -- disp
  poke p (RiscvOpMemRef b d) = do
     {#set riscv_op_mem->base#} p (fromIntegral b)
     {#set riscv_op_mem->disp#} p (fromIntegral d)

-- | Instruction operand
data CsRiscvOp -- cs_riscv_op
  = Reg Word32 -- register value for REG operand
  | Imm Int64  -- immediate value for IMM operand
  | Mem RiscvOpMemRef -- base/disp value for MEM operand
  deriving (Show, Eq)
  
instance Storable CsRiscvOp where
  sizeOf _ = {#sizeof cs_riscv_op#}
  alignment _ = {#alignof cs_riscv_op#}
  peek p = do
    case (toEnum (fromIntegral <$> {#get cs_riscv_op->riscv_op_type#})) of
      RiscvOpReg -> (Reg . fromIntegral) <$> {#get cs_riscv_op->reg#} p
      RiscvOpImm -> (Imm . fromIntegral) <$> {#get cs_riscv_op->imm#} p
      RiscvOpMem -> Mem <$> RiscvOpMemRef <$> (fromIntegral <$> {#get cs_riscv_op->mem.base#} p)
                                          <$> (fromIntegral <$> {#get cs_riscv_op->mem.disp#} p)
  poke p op = do
    let setType = {#set cs_riscv_op->type#} p . fromIntegral . fromEnum
    case op of
      Reg w -> do
          setType RiscvOpReg
          {#set cs_riscv_op->reg#} p (fromIntegral w)
      Imm i -> do
          setType RiscvOpImm
          {#set cs_riscv_op->reg#} p (fromIntegral i)
      Mem ref -> do
          setType RiscvOpMem
          {#set cs_riscv_op->mem.base#} p (fromIntegral (base ref))
          {#set cs_riscv_op->mem.disp#} p (fromIntegral (disp ref))

-- | Instruction structure
data CsRiscv = CsRiscv -- struct cs_riscv
  { needEffectiveAddr :: Bool -- Does this instruction need effective address or not
  , operands :: [CsRiscvOp]   -- operands for this instruction
  } deriving (Show_eq)

instance Storable CsRiscv where
  sizeOf _ = {#sizeof cs_riscv#}
  alignment _ = {#alignof cs_riscv#}
  peek p = CsRiscv
    <$> (toBool . fromIntegral <$> {#get cs_riscv->need_effective_addr#} p)
    <*> do num <- fromIntegral <$> {#get cs_riscv->op_count#} p
           let ptr = plusPtr p {#offsetof cs_riscv->operands#}
           peekArray num ptr -- operands
  poke p (CsRiscv nEA ops) = do
      {#set cs_riscv->need_effective_addr#} p (fromBool nEA)
      {#set cs_riscv->op_count#} p (fromIntegral $ length ops)
      let ptr = plusPtr p {#offsetof cs_riscv->operands#}
      pokeArray ptr ops

-- | RISCV registers
{#enum riscv_reg as RiscvReg {underscoreToCase}
   deriving (Show, Eq, Bounded)#}

-- | RISCV instruction
{#enum riscv_insn as RiscvInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}


-- | Group of RISCV instructions
{#enum riscv_insn_group RiscvInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
 
