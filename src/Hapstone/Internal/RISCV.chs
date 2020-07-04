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
{#enum riscv_op_type as RiscvOpType {underscoreToCase}
   deriving (Show, Eq, Bounded)#}

-- | RISC-V Instruction's operand referring to memory
--   This is associated with RISCV_OP_MEM operand type above
data RiscvOpMemStruct = RiscvOpMemStruct
  { base :: Word32 -- base register
  , disp :: Int64  -- displacement/offset vaue
  } deriving (Show, Eq)

-- | Instruction operand
data CsRiscvOpValue
  = Reg Word32 -- register value for REG operand
  | Imm Int64  -- immediate value for IMM operand
  | Mem RiscvOpMemStruct -- base/disp value for MEM operand
  deriving (Show, Eq)

data CsRiscvOpStruct = CsRiscvOpStruct
  { type :: RiscvOpType -- operand type
  , value :: CsRiscvOpValue
  } deriving (Show_eq)
  
-- | Instruction structure
data CsRiscvStruct = CsRiscvStruct
  { needEffectiveAddr :: Bool -- Does this instruction need effective address or not
  , opCount ::  Word8 -- Number of operands of this instruction, or 0 when instruction has no operand.
  , operands :: [CsRiscvOp] -- operands for this instruction
  } deriving (Show_eq)

-- | RISCV registers
{#enum riscv_reg as RiscvReg {underscoreToCase}
   deriving (Show, Eq, Bounded)#}

-- | RISCV instruction
{#enum riscv_insn as RiscvInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}


-- | Group of RISCV instructions
{#enum riscv_insn_group RiscvInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
 
