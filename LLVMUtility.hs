{-
******************************************************************************
*				   H M T C				                                     *
*									                                         *
*	Module:		LLVMUtility      					                         *
*	Purpose:	Utility functions for the LLVM Code Generator        	     *
*	Authors:	Michael B. Gale					                             *
*									                                         *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									                                         *
******************************************************************************
-}

module LLVMUtility (
    Value,
    Function,
    Builder,
    Global,
    FunBuilder,
    TAM,
    tamJump,
    tamCondJump,
    tamCall,
    tamPrompt,
    tamReadInt,
    tamWriteInt,
    tamBuildPow,
    tamGetStack,
    tamGetFun,
    tamSetFun,
    tamGetST,
    tamStore,
    tamAddr,
    tamAddrToIx,
    tamPush,
    tamPop,
    tamAdjust,
    tamLoad,
    tamMkOrGetLabel,
    tamReturn,
    tamBuilder,
    tamInsertAt,
    tamInit,
    tamMkFun,
    tamWord
) where

-- Standard library imports
import Control.Monad.State
import qualified Data.Map as M

-- HMTC module imports
import TAMCode
import LLVM.Base.Core
import LLVM.Base.Types
import LLVM.Base.Const
import LLVM.Base.Builder

------------------------------------------------------------------------------
-- TAM
------------------------------------------------------------------------------

type Function = ValueRef
type Builder  = BuilderRef
type Global   = ValueRef
type Value    = ValueRef
type Label    = BasicBlockRef
type Type     = TypeRef

type FunBuilder = Builder -> TAM ()

stackSize :: Int
stackSize = 1024

data TAMState = TAMState {
    tamLabels :: M.Map String Label,
    tamCFun   :: Function,
    tamStack  :: Global,
    tamSB     :: Global,
    tamLB     :: Global,
    tamST     :: Global
}

type TAM = StateT TAMState LLVMModule

tamGetLabels :: TAM (M.Map String Label)
tamGetLabels = gets tamLabels

tamSetLabels :: M.Map String Label -> TAM ()
tamSetLabels ls = modify $ \s -> s { tamLabels = ls }

tamGetFun :: TAM Function
tamGetFun = gets tamCFun

tamSetFun :: Function -> TAM ()
tamSetFun f = modify $ \s -> s { tamCFun = f } 

tamGetStack :: TAM Global
tamGetStack = gets tamStack

tamGetSB :: TAM Global
tamGetSB = gets tamSB

tamGetLB :: TAM Global
tamGetLB = gets tamLB

tamGetST :: TAM Global
tamGetST = gets tamST
    
tamMkFun :: String -> LLVMModule Function
tamMkFun n = do
    ft <- tamFunType
    llvmAddFunction n ft
    
tamMkStack :: Int -> LLVMModule Global
tamMkStack n = do
    wt <- llvmInt32Type
    wa <- llvmArrayType wt n
    sg <- llvmAddGlobal "stack" wa
    llvmSetGlobalConstant sg False
    si <- llvmConstNull wa
    llvmSetInitialiser sg si
    z  <- llvmInt32Const 0
    sp <- llvmConstInBoundsGEP sg [z,z]
    return sp

tamMkReg :: String -> LLVMModule Global
tamMkReg n = do
    -- create a global variable for the register
    wt <- llvmInt32Type
    rg <- llvmAddGlobal n wt
    llvmSetGlobalConstant rg False
    -- initialise it to zero
    ri <- llvmConstNull wt
    llvmSetInitialiser rg ri
    return rg
    
tamFunType :: LLVMModule Type
tamFunType = do
    vt <- llvmVoidType
    llvmMakeFunctionType False vt [] 
    
tamWordTy :: LLVMBuilder Type
tamWordTy = lift llvmInt32Type
    
tamWord :: Builder -> Int -> TAM Value
tamWord b v = tamBuilder b (lift (llvmInt32Const v))
    
------------------------------------------------------------------------------
-- Interface to Standard C Library
------------------------------------------------------------------------------
    
tamExternals :: LLVMModule ()
tamExternals = do
    vt <- llvmVoidType
    wt <- llvmInt32Type
    st <- llvmStringType
    dt <- llvmDoubleType
    -- int getchar()
    t1 <- llvmMakeFunctionType False wt []
    llvmAddFunction "getchar" t1
    -- int putchar(int)
    t2 <- llvmMakeFunctionType False wt [wt]
    llvmAddFunction "putchar" t2
    -- int printf(char*, ...)
    -- int scanf(char*, ...)
    t3 <- llvmMakeFunctionType True wt [st]
    llvmAddFunction "printf" t3
    llvmAddFunction "scanf" t3
    -- double pow(double, double)
    t4 <- llvmMakeFunctionType False dt [dt, dt]
    llvmAddFunction "pow" t4
    return ()
    
tamBuildPow :: Value -> Value -> LLVMBuilder Value
tamBuildPow base exp = do
    f  <- lift $ llvmGetNamedFunction "pow"
    dt <- lift $ llvmDoubleType
    bf <- llvmBuildSItoFP base dt
    ef <- llvmBuildSItoFP exp dt
    rf <- llvmBuildCall f [bf, ef]
    it <- tamWordTy
    r  <- llvmBuildFPtoSI rf it
    return r
    
tamPrompt :: String -> LLVMBuilder ()
tamPrompt xs = do
    f <- lift $ llvmGetNamedFunction "printf"
    g <- llvmBuildGlobalStringPtr xs
    llvmBuildCall f [g]
    return ()
    
tamReadInt :: LLVMBuilder Value
tamReadInt = do
    f <- lift $ llvmGetNamedFunction "scanf"
    g <- llvmBuildGlobalStringPtr "%d"
    r <- lift $ tamMkReg "ib"
    llvmBuildCall f [g, r]
    v <- llvmBuildLoad "" r
    return v
    
tamWriteInt :: Value -> LLVMBuilder ()
tamWriteInt v = do
    f <- lift $ llvmGetNamedFunction "printf"
    g <- llvmBuildGlobalStringPtr "Output: %d\n"
    llvmBuildCall f [g,v]
    return ()
    
tamDebug :: String -> [Value] -> LLVMBuilder ()
tamDebug msg vs = do
    f <- lift $ llvmGetNamedFunction "printf"
    g <- llvmBuildGlobalStringPtr msg
    llvmBuildCall f (g : vs)
    return ()
  
------------------------------------------------------------------------------
-- Control flow
------------------------------------------------------------------------------
  
tamJump :: Builder -> Label -> TAM ()
tamJump b l = tamBuilder b $ llvmBuildBr l >> return ()
  
tamCondJump :: Builder -> Label -> Label -> TAM ()
tamCondJump b t f = do
    -- pop argument off the stack, v : i32
    v <- tamPop b
    tamBuilder b $ do
        -- cast to i1
        i <- lift $ llvmInt1Type
        c <- llvmBuildTrunc v i
        -- conditional jump if c is true
        llvmBuildCondBr c t f
    return ()
  
tamCall :: String -> Builder -> TAM ()
tamCall n b = tamBuilder b $ do
    f <- lift $ llvmGetNamedFunction ('_' : n)
    llvmBuildCall f []
    return ()

tamMkOrGetLabel' :: String -> M.Map String Label -> TAM Label
tamMkOrGetLabel' n ls = case M.lookup n ls of
    (Just l) -> return l
    Nothing  -> do
        f <- tamGetFun
        l <- lift $ llvmAppendBasicBlock f n
        tamSetLabels (M.insert n l ls)
        return l
    
tamMkOrGetLabel :: String -> TAM Label
tamMkOrGetLabel n = do
    ls <- tamGetLabels
    tamMkOrGetLabel' n ls

tamReturn :: Builder -> TAM ()
tamReturn b = tamBuilder b llvmBuildRetVoid >> return ()
    
------------------------------------------------------------------------------
-- Addressing
------------------------------------------------------------------------------
   
tamAddr :: Builder -> Addr -> TAM Value
tamAddr b (SB d) = tamBuilder b $ lift $ llvmInt32Const d
tamAddr b (LB d) = do
    lbp <- tamGetLB
    tamBuilder b $ do
        lb <- llvmBuildLoad "" lbp
        dv <- lift $ llvmInt32Const d
        llvmBuildAdd lb dv
tamAddr b (ST d) = do
    stp <- tamGetST
    tamBuilder b $ do
        st <- llvmBuildLoad "" stp
        dv <- lift $ llvmInt32Const d
        llvmBuildAdd st dv
   
tamAddrToIx :: Builder -> Addr -> TAM Value
tamAddrToIx b (SB d) = do
    sbp <- tamGetSB
    tamBuilder b $ do
        sb <- llvmBuildLoad "" sbp
        dv <- lift $ llvmInt32Const d
        rv <- llvmBuildAdd sb dv
        return rv
tamAddrToIx b (LB d) = do
    stp <- tamGetST
    lbp <- tamGetLB
    tamBuilder b $ do
        st <- llvmBuildLoad "" stp
        lb <- llvmBuildLoad "" lbp
        dv <- lift $ llvmInt32Const d
        ec <- lift $ llvmInt32Const 1
        lr <- llvmBuildSub st ec
        rr <- llvmBuildAdd lb dv
        llvmBuildSub lr rr
tamAddrToIx b (ST d) = do
    tamBuilder b $ do
        dv <- lift $ llvmInt32Const d
        ec <- lift $ llvmInt32Const 1
        er <- llvmBuildAdd ec dv
        llvmBuildNeg er
   
------------------------------------------------------------------------------
-- Stack operations
------------------------------------------------------------------------------
   
tamPush :: Builder -> Value -> TAM ()
tamPush b v = do
    stp <- tamGetST
    stk <- tamGetStack
    tamBuilder b $ do 
        st <- llvmBuildLoad "" stp
        -- store value
        stl <- llvmBuildGEP stk [st]
        llvmBuildStore v stl
        -- inc. stack pointer
        si <- lift $ llvmInt32Const 1
        sr <- llvmBuildAdd st si
        llvmBuildStore sr stp
    return ()
   
tamPop :: Builder -> TAM Value
tamPop b = do
    stp <- tamGetST
    stk <- tamGetStack
    tamBuilder b $ do
        st <- llvmBuildLoad "" stp
        -- dec. stack pointer
        si <- lift $ llvmInt32Const 1
        st <- llvmBuildSub st si
        llvmBuildStore st stp
        -- load value
        stl <- llvmBuildGEP stk [st]
        v   <- llvmBuildLoad "" stl
        return v
 
tamAdjust :: Builder -> Int -> Int -> TAM ()
tamAdjust b m n = do
    stp <- tamGetST
    stk <- tamGetStack
    tamBuilder b $ do
        st <- llvmBuildLoad "" stp
        -- move m values up n positions
        -- TODO: since MT doesn't support functions 
        -- this isn't needed
        -- dec. stack pointer
        si <- lift $ llvmInt32Const n
        sr <- llvmBuildSub st si
        llvmBuildStore sr stp
    return ()

tamStore :: Builder -> Value -> Value -> TAM ()
tamStore b i v = do
    stk <- tamGetStack
    tamBuilder b $ do
        stl <- llvmBuildGEP stk [i]
        llvmBuildStore v stl
    return ()
    
tamLoad :: Builder -> Value -> TAM Value
tamLoad b p = do
    stk <- tamGetStack
    tamBuilder b $ do
        stl <- llvmBuildGEP stk [p]
        llvmBuildLoad "" stl
    
------------------------------------------------------------------------------
-- Initialisation and helper functions
------------------------------------------------------------------------------
    
tamBuilder :: Builder -> LLVMBuilder a -> TAM a
tamBuilder b = lift . llvmUsingBuilder b

tamInsertAt :: Builder -> Label -> TAM ()
tamInsertAt b l = lift $ llvmPositionBuilderAtEnd b l

tamFun :: String -> (Function -> TAMState) -> FunBuilder -> LLVMModule ()
tamFun n g m = do
    f  <- tamMkFun n
    bb <- llvmAppendBasicBlock f "entry"
    b  <- llvmMakeBuilder bb
    evalStateT (m b) (g f)
    
tamStateGen :: Global -> Global -> Global -> Global -> Function -> TAMState
tamStateGen sk sb lb st f = TAMState M.empty f sk sb lb st
    
tamInit :: String -> [TAMInst] -> (Builder -> [TAMInst] -> TAM ()) -> [(String, FunBuilder)] -> LLVMModule () 
tamInit n is m ms = do
    tamExternals
    sk <- tamMkStack stackSize
    sb <- tamMkReg "sb"
    lb <- tamMkReg "lb"
    st <- tamMkReg "st"
    mapM_ (\(n,f) -> tamFun ('_' : n) (tamStateGen sk sb lb st) f) ms
    tamFun "main" (tamStateGen sk sb lb st) (\b -> m b is)
    llvmWriteModuleToFile (n ++ ".bc")
    