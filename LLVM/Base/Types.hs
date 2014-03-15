{--------------------------------------------------------------------------------------------------
                                   LLVM Bindings for Haskell                                     
                                     Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

module LLVM.Base.Types (
    llvmTypeOf,
    llvmVoidType,
    llvmInt1Type,
    llvmInt8Type,
    llvmInt16Type,
    llvmInt32Type,
    llvmInt64Type,
    llvmIntType,
    llvmGlInt8Type,
    llvmFloatType,
    llvmDoubleType,
    llvmMakeFunctionType,
    llvmArrayType,
    llvmPointerType,
    llvmStringType,
    llvmInt8PtrType,
    llvmInt32PtrType,
    FFI.TypeRef
)
where

  {----------------------------------------------------------------------}
  {-- Module Imports                                                    -}
  {----------------------------------------------------------------------}

import Control.Monad.State (get, lift, liftIO)
  
import Foreign.C.String (withCString)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fromBool)
  
import qualified LLVM.Base.FFI.Core as FFI
  
import LLVM.Base.Core

  {----------------------------------------------------------------------}
  {-- Misc Types                                                        -}
  {----------------------------------------------------------------------}
  
llvmTypeOf :: FFI.ValueRef -> LLVMModule FFI.TypeRef
llvmTypeOf v = liftIO $ FFI.llvmTypeOf v
  
llvmVoidType :: LLVMModule FFI.TypeRef
llvmVoidType = liftIO $ FFI.llvmVoidType
  
  {----------------------------------------------------------------------}
  {-- Integer Types                                                     -}
  {----------------------------------------------------------------------}
  
llvmInt1Type :: LLVMModule FFI.TypeRef
llvmInt1Type = lift $ get >>= \c -> liftIO $ FFI.llvmInt1Type c
  
llvmInt8Type :: LLVMModule FFI.TypeRef
llvmInt8Type = lift $ get >>= \c -> liftIO $ FFI.llvmInt8Type c
  
llvmInt16Type :: LLVMModule FFI.TypeRef
llvmInt16Type = lift $ get >>= \c -> liftIO $ FFI.llvmInt16Type c
  
llvmInt32Type :: LLVMModule FFI.TypeRef
llvmInt32Type = lift $ get >>= \c -> liftIO $ FFI.llvmInt32Type c
  
llvmInt64Type :: LLVMModule FFI.TypeRef
llvmInt64Type = lift $ get >>= \c -> liftIO $ FFI.llvmInt64Type c
  
llvmIntType :: Int -> LLVMModule FFI.TypeRef
llvmIntType s = lift $ get >>= \c -> liftIO $ FFI.llvmIntType c $ fromIntegral s

llvmGlInt8Type :: LLVMModule FFI.TypeRef
llvmGlInt8Type = liftIO $ FFI.llvmGlInt8Type

{----------------------------------------------------------------------}
{-- Real Types                                                        -}
{----------------------------------------------------------------------}

llvmFloatType :: LLVMModule FFI.TypeRef
llvmFloatType = lift $ get >>= \c -> liftIO $ FFI.llvmFloatType c

llvmDoubleType :: LLVMModule FFI.TypeRef
llvmDoubleType = lift $ get >>= \c -> liftIO $ FFI.llvmDoubleType c

{----------------------------------------------------------------------}
{-- Function Types                                                    -}
{----------------------------------------------------------------------}
  
llvmMakeFunctionType :: Bool -> FFI.TypeRef -> [FFI.TypeRef] -> LLVMModule FFI.TypeRef
llvmMakeFunctionType v r ts = liftIO $ withArrayLen ts $ 
    \len ptr -> FFI.llvmCreateFunctionType r ptr (fromIntegral len) (fromBool v)

  {----------------------------------------------------------------------}
  {-- Pointer Types                                                     -}
  {----------------------------------------------------------------------}
  
llvmArrayType :: FFI.TypeRef -> Int -> LLVMModule FFI.TypeRef
llvmArrayType t s = liftIO $ FFI.llvmArrayType t (fromIntegral s)
  
llvmPointerType :: FFI.TypeRef -> LLVMModule FFI.TypeRef
llvmPointerType t = liftIO $ FFI.llvmPointerType t 0

llvmStringType :: LLVMModule FFI.TypeRef
llvmStringType = llvmGlInt8Type >>= llvmPointerType

llvmInt8PtrType :: LLVMModule FFI.TypeRef
llvmInt8PtrType= llvmInt8Type >>= llvmPointerType

llvmInt32PtrType :: LLVMModule FFI.TypeRef
llvmInt32PtrType = llvmInt32Type >>= llvmPointerType

{--------------------------------------------------------------------------------------------------
                                          End of File                                            
--------------------------------------------------------------------------------------------------}          
