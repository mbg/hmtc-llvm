{-
******************************************************************************
*				   H M T C				                                     *
*									                                         *
*	Module:		LLVMCodeGenerator					                         *
*	Purpose:	Generate LLVM Byte Code from                                 *
*                       MiniTriangle Intermediate Representation (MTIR)	     *
*	Authors:	Michael B. Gale					                             *
*									                                         *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									                                         *
******************************************************************************
-}

module LLVMCodeGenerator (
    genLLVMCode	-- :: String -> [TAMInst] -> IO ()
) where

import Control.Monad.State

-- HMTC module imports
import TAMCode
import LLVMUtility
import LLVM.Base.Core
import LLVM.Base.Types
import LLVM.Base.Const
import LLVM.Base.Builder

------------------------------------------------------------------------------
-- Initialisation of the LLVM Code Generator
------------------------------------------------------------------------------

genLLVMCode :: String -> [TAMInst] -> IO ()
genLLVMCode n is = llvmWithContext (genModule n is)

genModule :: String -> [TAMInst] -> LLVMContext ()
genModule n is = llvmWithModule n (tamInit n is comp tamLibrary)

------------------------------------------------------------------------------
-- MiniTriangle library functions
------------------------------------------------------------------------------

tamLibrary :: [(String, FunBuilder)]
tamLibrary = [
    ("add",    tamAdd),
    ("sub",    tamSub),
    ("mul",    tamMul),
    ("div",    tamDiv),
    ("pow",    tamPow),
    ("neg",    tamNeg),
    ("lt",     tamLT),
    ("le",     tamLE),
    ("eq",     tamEQ),
    ("ne",     tamNE),
    ("ge",     tamGE),    
    ("gt",     tamGT),
    ("and",    tamAnd),
    ("or",     tamOr),
    ("not",    tamNot),
    ("getint", tamGetInt),
    ("putint", tamPutInt)]
    
tamUnaryOp :: (Value -> LLVMBuilder Value) -> FunBuilder
tamUnaryOp f b = do
    -- pop an argument off the stack
    v <- tamPop b
    -- build the instruction
    r <- tamBuilder b (f v)
    -- push the result onto the stack
    tamPush b r
    -- return
    tamBuilder b llvmBuildRetVoid
    return ()
    
tamBinaryOp :: (Value -> Value -> LLVMBuilder Value) -> FunBuilder
tamBinaryOp f b = do
    -- pop two arguments off the stack
    y <- tamPop b
    x <- tamPop b
    -- build the instruction
    r <- tamBuilder b (f x y)
    -- push the result onto the stack
    tamPush b r
    -- return
    tamBuilder b llvmBuildRetVoid
    return ()
    
---- arithmetic functions ----
    
tamAdd :: FunBuilder
tamAdd = tamBinaryOp llvmBuildAdd

tamSub :: FunBuilder
tamSub = tamBinaryOp llvmBuildSub

tamMul :: FunBuilder
tamMul = tamBinaryOp llvmBuildMul

tamDiv :: FunBuilder
tamDiv = tamBinaryOp llvmBuildUDiv

tamPow :: FunBuilder
tamPow = tamBinaryOp tamBuildPow

tamNeg :: FunBuilder
tamNeg = tamUnaryOp llvmBuildNeg

---- comparison functions ----

tamCompare :: IntPred -> FunBuilder
tamCompare p b = do
    -- pop two arguments off the stack
    y <- tamPop b
    x <- tamPop b
    r <- tamBuilder b $ do
        -- compare the two (signed), v : i1
        v <- llvmBuildICmp p x y
        -- zero-extend v to an i32
        t <- lift $ llvmInt32Type
        llvmBuildZExt v t
    -- push the result onto the stack
    tamPush b r
    -- return
    tamBuilder b llvmBuildRetVoid
    return ()
    
tamLT :: FunBuilder
tamLT = tamCompare IntSLT

tamLE :: FunBuilder
tamLE = tamCompare IntSLE
  
tamEQ :: FunBuilder
tamEQ = tamCompare IntEQ

tamNE :: FunBuilder
tamNE = tamCompare IntNE

tamGE :: FunBuilder
tamGE = tamCompare IntSGE
  
tamGT :: FunBuilder
tamGT = tamCompare IntSGT

---- logic functions ----

tamAnd :: FunBuilder
tamAnd = tamBinaryOp llvmBuildAnd

tamOr :: FunBuilder
tamOr = tamBinaryOp llvmBuildOr

tamNot :: FunBuilder
tamNot = tamUnaryOp llvmBuildNot

---- IO functions ----

tamGetInt :: FunBuilder
tamGetInt b = do
    -- get the pointer to the stack
    stk <- tamGetStack
    -- pop address off the stack
    av  <- tamPop b
    tamBuilder b $ do
        -- calculate the address of the value in the
        -- stack at address av
        ap <- llvmBuildGEP stk [av]
        -- prompt user and retrieve a value from the
        -- standard input
        tamPrompt "Enter integer:\n"
        v <- tamReadInt
        -- store the value in the stack at address ap
        llvmBuildStore v ap
        -- return
        llvmBuildRetVoid
    return ()
    
tamPutInt :: FunBuilder
tamPutInt b = do
    -- pop a value off the stack
    v <- tamPop b
    tamBuilder b $ do
        -- write the value to the standard output
        tamWriteInt v
        -- return
        llvmBuildRetVoid
    return ()

------------------------------------------------------------------------------
-- Code generation functions
------------------------------------------------------------------------------
  
compAux :: Builder -> [TAMInst] -> TAM ()
compAux b []             = return ()
compAux b (LABEL n : is) = do
    l <- tamMkOrGetLabel n
    tamInsertAt b l
    comp b is
compAux b (_ : is) = compAux b is
   
comp :: Builder -> [TAMInst] -> TAM ()
comp b []             = return ()
comp b (LABEL n : is) = do
    l <- tamMkOrGetLabel n
    tamJump b l
    tamInsertAt b l
    comp b is
comp b (LOADL l : is) = do
    v <- tamWord b l
    tamPush b v
    comp b is
comp b (LOAD a : is) = do
    p <- tamAddrToIx b a
    v <- tamLoad b p
    tamPush b v
    comp b is
comp b (LOADA a : is) = do
    v <- tamAddr b a
    tamPush b v
    comp b is
comp b (STORE a : is) = do
    i <- tamAddrToIx b a
    v <- tamPop b
    tamStore b i v
    comp b is
comp b (CALL n : is) = do
    tamCall n b
    comp b is
comp b (POP m n : is) = do
    tamAdjust b m n
    comp b is
comp b (JUMP n : is) = do
    l <- tamMkOrGetLabel n
    tamJump b l
    compAux b is
comp b (JUMPIFZ n : is) = do
    t <- tamMkOrGetLabel ""
    f <- tamMkOrGetLabel n
    tamCondJump b t f
    tamInsertAt b t
    comp b is
comp b (JUMPIFNZ n : is) = do
    t <- tamMkOrGetLabel n
    f <- tamMkOrGetLabel ""
    tamCondJump b t f
    tamInsertAt b f
    comp b is
comp b (HALT : is) = do
    tamReturn b
    comp b is
comp b (_ : is) = do
    comp b is


