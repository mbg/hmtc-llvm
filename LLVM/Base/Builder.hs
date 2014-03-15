{--------------------------------------------------------------------------------------------------
                                   LLVM Bindings for Haskell                                     
                                     Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

module LLVM.Base.Builder (
	LLVMBuilder,
	llvmMakeBuilder,
	llvmDisposeBuilder,
	llvmPositionBuilderAtEnd,
	llvmWithBuilder,
	llvmUsingBuilder,
	llvmBuildRetVoid,
	llvmBuildBr,
	llvmBuildCondBr,
	llvmBuildAdd,
	llvmBuildSub,
	llvmBuildMul,
	llvmBuildUDiv,
	llvmBuildAnd,
	llvmBuildOr,
	llvmBuildNeg,
	llvmBuildNot,
	llvmBuildICmp,
	llvmBuildTrunc,
	llvmBuildZExt,
	llvmBuildFPtoUI,
	llvmBuildFPtoSI,
	llvmBuildUItoFP,
	llvmBuildSItoFP,
	llvmBuildAlloca,
	llvmBuildStore,
	llvmBuildGEP,
	llvmBuildInBoundsGEP,
	llvmBuildGlobalString,
	llvmBuildGlobalStringPtr,
	llvmBuildLoad,
	llvmBuildPtrToInt,
	llvmBuildIntToPtr,
	llvmBuildCall,
	FFI.IntPred(..),
	FFI.BuilderRef,
	FFI.BasicBlockRef
) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}
  
import Control.Monad.State
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Array (withArrayLen)
  
import qualified LLVM.Base.FFI.Core as FFI

import LLVM.Base.Core

{----------------------------------------------------------------------}
{-- LLVM Builder                                                      -}
{----------------------------------------------------------------------}
  
type LLVMBuilder = StateT FFI.BuilderRef LLVMModule

llvmMakeBuilder :: FFI.BasicBlockRef -> LLVMModule FFI.BuilderRef
llvmMakeBuilder e = do
    b <- liftIO $ FFI.llvmCreateBuilder
    liftIO $ FFI.llvmPositionBuilderAtEnd b e
    return b

llvmDisposeBuilder :: FFI.BuilderRef -> LLVMModule ()
llvmDisposeBuilder b = liftIO $ FFI.llvmDisposeBuilder b 

llvmPositionBuilderAtEnd :: FFI.BuilderRef -> FFI.BasicBlockRef -> LLVMModule ()
llvmPositionBuilderAtEnd b p = liftIO $ FFI.llvmPositionBuilderAtEnd b p

llvmWithBuilder :: FFI.BasicBlockRef -> LLVMBuilder a -> LLVMModule a
llvmWithBuilder e f = do
    b <- liftIO $ FFI.llvmCreateBuilder
    liftIO $ FFI.llvmPositionBuilderAtEnd b e
    r <- evalStateT f b
    liftIO $ FFI.llvmDisposeBuilder b
    return r

llvmUsingBuilder :: FFI.BuilderRef -> LLVMBuilder a -> LLVMModule a
llvmUsingBuilder b f = do
    r <- evalStateT f b
    return r

{----------------------------------------------------------------------}
{-- LLVM Control Instructions                                         -}
{----------------------------------------------------------------------}
  
llvmBuildRetVoid :: LLVMBuilder FFI.ValueRef
llvmBuildRetVoid = get >>= 
    \b -> liftIO $ FFI.llvmBuildRetVoid b

llvmBuildBr :: FFI.BasicBlockRef -> LLVMBuilder FFI.ValueRef
llvmBuildBr t = get >>=
    \b -> liftIO $ FFI.llvmBuildBr b t

llvmBuildCondBr :: FFI.ValueRef -> FFI.BasicBlockRef -> FFI.BasicBlockRef -> LLVMBuilder FFI.ValueRef
llvmBuildCondBr v t f = get >>=
    \b -> liftIO $ FFI.llvmBuildCondBr b v t f

llvmBuildCall :: FFI.ValueRef -> [FFI.ValueRef] -> LLVMBuilder FFI.ValueRef
llvmBuildCall f ps = get >>=
    \b       -> liftIO $ withArrayLen ps $
    \len ptr -> withCString "" $
    \s       -> FFI.llvmBuildCall b f ptr (fromIntegral len) s

{----------------------------------------------------------------------}
{-- LLVM Arithmetic Instructions                                      -}
{----------------------------------------------------------------------}
  
{-|
  Procedures in the LLVM API which generate arithmetic instructions with
  two arguments all have this type.
-}  
type LLVMArithFun = FFI.BuilderRef -> FFI.ValueRef -> FFI.ValueRef -> CString -> IO FFI.ValueRef
  
{-|
  We can now generalise the computation which calls such a procedure.
-}
llvmBuildArith :: LLVMArithFun -> FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildArith f l r = get >>=
    \b -> liftIO $ withCString "" $
    \s -> f b l r s
  
llvmBuildAdd :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildAdd = llvmBuildArith FFI.llvmBuildAdd

llvmBuildSub :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildSub = llvmBuildArith FFI.llvmBuildSub

llvmBuildMul :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildMul = llvmBuildArith FFI.llvmBuildMul

llvmBuildUDiv :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildUDiv = llvmBuildArith FFI.llvmBuildUDiv 

llvmBuildAnd :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildAnd = llvmBuildArith FFI.llvmBuildAnd

llvmBuildOr :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildOr = llvmBuildArith FFI.llvmBuildOr

llvmBuildNeg :: FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildNeg v = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildNeg b v s
  
llvmBuildNot :: FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildNot v = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildNot b v s
  
llvmBuildICmp :: FFI.IntPred -> FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildICmp p l r = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildICmp b (FFI.fromIntPred p) l r s
  
llvmBuildTrunc :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildTrunc v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildTrunc b v t s
  
llvmBuildZExt :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildZExt v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildZExt b v t s

llvmBuildFPtoUI :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildFPtoUI v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildFPtoUI b v t s

llvmBuildFPtoSI :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildFPtoSI v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildFPtoSI b v t s

llvmBuildUItoFP :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildUItoFP v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildUItoFP b v t s

llvmBuildSItoFP :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildSItoFP v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildSItoFP b v t s
  
{----------------------------------------------------------------------}
{-- LLVM Memory Instructions                                          -}
{----------------------------------------------------------------------}
  
llvmBuildAlloca :: String -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildAlloca n t = get >>=
    \b -> liftIO $ withCString n $
    \s -> FFI.llvmBuildAlloca b t s

llvmBuildStore :: FFI.ValueRef -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildStore v p = get >>=
    \b -> liftIO $ FFI.llvmBuildStore b v p

llvmBuildGEP :: FFI.ValueRef -> [FFI.ValueRef] -> LLVMBuilder FFI.ValueRef
llvmBuildGEP p is = get >>=
    \b       -> liftIO $ withArrayLen is $
    \len ptr -> withCString "" $
    \s       -> FFI.llvmBuildGEP b p ptr (fromIntegral len) s

llvmBuildInBoundsGEP :: FFI.ValueRef -> [FFI.ValueRef] -> LLVMBuilder FFI.ValueRef
llvmBuildInBoundsGEP p is = get >>=
    \b       -> liftIO $ withArrayLen is $
    \len ptr -> withCString "" $
    \s       -> FFI.llvmBuildInBoundsGEP b p ptr (fromIntegral len) s

llvmBuildGlobalString :: String -> LLVMBuilder FFI.ValueRef
llvmBuildGlobalString xs = get >>=
    \b -> liftIO $ withCString xs $
    \v -> withCString "" $
    \n -> FFI.llvmBuildGlobalString b v n

llvmBuildGlobalStringPtr :: String -> LLVMBuilder FFI.ValueRef
llvmBuildGlobalStringPtr xs = get >>=
    \b -> liftIO $ withCString xs $
    \v -> withCString "" $
    \n -> FFI.llvmBuildGlobalStringPtr b v n

llvmBuildLoad :: String -> FFI.ValueRef -> LLVMBuilder FFI.ValueRef
llvmBuildLoad n p = get >>=
    \b -> liftIO $ withCString n $
    \s -> FFI.llvmBuildLoad b p s

llvmBuildPtrToInt :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildPtrToInt v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildPtrToInt b v t s

llvmBuildIntToPtr :: FFI.ValueRef -> FFI.TypeRef -> LLVMBuilder FFI.ValueRef
llvmBuildIntToPtr v t = get >>=
    \b -> liftIO $ withCString "" $
    \s -> FFI.llvmBuildIntToPtr b v t s

{--------------------------------------------------------------------------------------------------
                                          End of File                                            
--------------------------------------------------------------------------------------------------}          
