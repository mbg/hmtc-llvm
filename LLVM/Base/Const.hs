{--------------------------------------------------------------------------------------------------
                                   LLVM Bindings for Haskell                                     
                                     Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

{-|
    This module provides an interface to the LLVM API for constants.
-}
module LLVM.Base.Const (
	llvmInt8Const,
	llvmInt32Const,
	llvmInt64Const,
	llvmConstArray,
	llvmConstString,
	llvmConstAdd,
	llvmConstSub,
	llvmConstPtrToInt,
	llvmConstGEP,
	llvmConstInBoundsGEP,
	llvmConstNamedStruct,
	llvmConstNull
) where

  {----------------------------------------------------------------------}
  {-- Module Imports                                                    -}
  {----------------------------------------------------------------------}
  
import Control.Monad.State (liftIO)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Marshal.Array (withArrayLen)
  
import qualified LLVM.Base.FFI.Core as FFI

import LLVM.Base.Core
import LLVM.Base.Types

  {----------------------------------------------------------------------}
  {-- Constants                                                         -}
  {----------------------------------------------------------------------}

llvmInt8Const :: Int -> LLVMModule FFI.ValueRef
llvmInt8Const v = llvmInt8Type >>=
    \t -> liftIO $ FFI.llvmConstInt t (fromIntegral v) (fromBool True)
  
llvmInt32Const :: Int -> LLVMModule FFI.ValueRef
llvmInt32Const v = llvmInt32Type >>=
    \t -> liftIO $ FFI.llvmConstInt t (fromIntegral v) (fromBool True)
  
llvmInt64Const :: Int -> LLVMModule FFI.ValueRef
llvmInt64Const v = llvmInt64Type >>=
    \t -> liftIO $ FFI.llvmConstInt t (fromIntegral v) (fromBool True)

llvmConstArray :: FFI.TypeRef -> [FFI.ValueRef] -> Int -> LLVMModule FFI.ValueRef
llvmConstArray t vs s = liftIO $ withArrayLen vs $
    \len ptr -> FFI.llvmConstArray t ptr (fromIntegral s)

llvmConstString :: String -> LLVMModule FFI.ValueRef
llvmConstString xs = liftIO $ withCString xs $ 
    \s -> FFI.llvmConstString s (fromIntegral (length xs)) (fromBool False)

  {----------------------------------------------------------------------}
  {-- Functions on Constants                                            -}
  {----------------------------------------------------------------------}

llvmConstAdd :: FFI.ValueRef -> FFI.ValueRef -> LLVMModule FFI.ValueRef
llvmConstAdd l r = liftIO $ FFI.llvmConstAdd l r

llvmConstSub :: FFI.ValueRef -> FFI.ValueRef -> LLVMModule FFI.ValueRef
llvmConstSub l r = liftIO $ FFI.llvmConstSub l r
  
llvmConstPtrToInt :: FFI.ValueRef -> FFI.TypeRef -> LLVMModule FFI.ValueRef
llvmConstPtrToInt v t = liftIO $ FFI.llvmConstPtrToInt v t

llvmConstGEP :: FFI.ValueRef -> [FFI.ValueRef] -> LLVMModule FFI.ValueRef
llvmConstGEP p is = liftIO $ withArrayLen is $
    \len ptr -> FFI.llvmConstGEP p ptr (fromIntegral len)

llvmConstInBoundsGEP :: FFI.ValueRef -> [FFI.ValueRef] -> LLVMModule FFI.ValueRef
llvmConstInBoundsGEP p is = liftIO $ withArrayLen is $
    \len ptr -> FFI.llvmConstGEP p ptr (fromIntegral len)

llvmConstNamedStruct :: FFI.TypeRef -> [FFI.ValueRef] -> LLVMModule FFI.ValueRef
llvmConstNamedStruct t ps = liftIO $ withArrayLen ps $ 
    \len ptr -> FFI.llvmConstNamedStruct t ptr (fromIntegral len)

llvmConstNull :: FFI.TypeRef -> LLVMModule FFI.ValueRef
llvmConstNull t = liftIO $ FFI.llvmConstNull t
  
{--------------------------------------------------------------------------------------------------
                                          End of File                                            
--------------------------------------------------------------------------------------------------}  