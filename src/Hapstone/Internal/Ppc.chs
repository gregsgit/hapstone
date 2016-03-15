{-# LANGUAGE ForeignFunctionInterface #-}
module Hapstone.Internal.Ppc where

#include <capstone/ppc.h>

{#context lib = "capstone"#}

import Foreign
import Foreign.C.Types

{#enum ppc_bc as PpcBc {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum ppc_bh as PpcBh {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum ppc_reg as PpcReg {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

{#enum ppc_op_type as PpcOpType {underscoreToCase}
    deriving (Show, Eq, Bounded)#}

data PpcOpMemStruct = PpcOpMemStruct PpcReg Int32
    deriving (Show, Eq)

instance Storable PpcOpMemStruct where
    sizeOf _ = {#sizeof ppc_op_mem#}
    alignment _ = {#alignof ppc_op_mem#}
    peek p = PpcOpMemStruct
        <$> ((toEnum . fromIntegral) <$> {#get ppc_op_mem->base#} p)
        <*> (fromIntegral <$> {#get ppc_op_mem->disp#} p)
    poke p (PpcOpMemStruct b d) = do
        {#set ppc_op_mem->base#} p (fromIntegral $ fromEnum b)
        {#set ppc_op_mem->disp#} p (fromIntegral d)

data PpcOpCrxStruct = PpcOpCrxStruct Word32 PpcReg PpcBc
    deriving (Show, Eq)

instance Storable PpcOpCrxStruct where
    sizeOf _ = {#sizeof ppc_op_crx#}
    alignment _ = {#alignof ppc_op_crx#}
    peek p = PpcOpCrxStruct
        <$> (fromIntegral <$> {#get ppc_op_crx->scale#} p)
        <*> ((toEnum . fromIntegral) <$> {#get ppc_op_crx->reg#} p)
        <*> ((toEnum . fromIntegral) <$> {#get ppc_op_crx->cond#} p)
    poke p (PpcOpCrxStruct s r c) = do
        {#set ppc_op_crx->scale#} p (fromIntegral s)
        {#set ppc_op_crx->reg#} p (fromIntegral $ fromEnum r)
        {#set ppc_op_crx->cond#} p (fromIntegral $ fromEnum c)

data CsPpcOp
    = Reg PpcReg
    | Imm Int32
    | Mem PpcOpMemStruct
    | Crx PpcOpCrxStruct
    | Undefined
    deriving (Show, Eq)

instance Storable CsPpcOp where
    sizeOf _ = 16
    alignment _ = 4
    peek p = do
        t <- fromIntegral <$> {#get cs_ppc_op->type#} p
        let bP = plusPtr p 4
        case toEnum t of
          PpcOpReg -> (Reg . toEnum . fromIntegral) <$> (peek bP :: IO CInt)
          PpcOpImm -> Imm <$> peek bP
          PpcOpMem -> Mem <$> peek bP
          PpcOpCrx -> Crx <$> peek bP
          _ -> return Undefined
    poke p op = do
        let bP = plusPtr p 4
            setType = {#set cs_ppc_op->type#} p . fromIntegral . fromEnum
        case op of
          Reg r -> do
              poke bP (fromIntegral $ fromEnum r :: CInt)
              setType PpcOpReg
          Imm i -> do
              poke bP i
              setType PpcOpImm
          Mem m -> do
              poke bP m
              setType PpcOpMem
          Crx c -> do
              poke bP c
              setType PpcOpCrx
          _ -> setType PpcOpInvalid

data CsPpc = CsPpc 
    { bc :: PpcBc
    , bh :: PpcBh
    , updateCr0 :: Bool
    , operands :: [CsPpcOp]
    } deriving (Show, Eq)

instance Storable CsPpc where
    sizeOf _ = 140
    alignment _ = 4
    peek p = CsPpc
        <$> ((toEnum . fromIntegral) <$> {#get cs_ppc->bc#} p)
        <*> ((toEnum . fromIntegral) <$> {#get cs_ppc->bh#} p)
        <*> (toBool <$> (peekByteOff p 8 :: IO Word8)) -- update_cr0
        <*> do num <- fromIntegral <$> {#get cs_ppc->op_count#} p
               let ptr = plusPtr p {#offsetof cs_ppc.operands#}
               peekArray num ptr
    poke p (CsPpc bc bh u o) = do
        {#set cs_ppc->bc#} p (fromIntegral $ fromEnum bc)
        {#set cs_ppc->bh#} p (fromIntegral $ fromEnum bh)
        pokeByteOff p 8 (fromBool u :: Word8) -- update_cr0
        {#set cs_ppc->op_count#} p (fromIntegral $ length o)
        if length o > 8
           then error "operands overflew 8 elements"
           else pokeArray (plusPtr p {#offsetof cs_ppc->operands#}) o

{#enum ppc_insn as PpcInsn {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
{#enum ppc_insn_group as PpcInsnGroup {underscoreToCase}
    deriving (Show, Eq, Bounded)#}
