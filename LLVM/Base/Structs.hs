{--------------------------------------------------------------------------------------------------
                                     LLVM Bindings for Haskell                                     
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

{-| 
	This module contains low-level bindings for LLVM functions which 
	deal with structures. 
-}
module LLVM.Base.Structs (
  llvmMakeStructType,
  llvmMakeNamedStruct,
  llvmSetStructBody
) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}
    
import qualified LLVM.Base.FFI.Core as FFI

import Control.Monad.State

import Foreign.C.String (withCString)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fromBool)
    
import LLVM.Base.Core
import LLVM.Base.Types

{----------------------------------------------------------------------}
{-- Structure Types                                                   -}
{----------------------------------------------------------------------}
    
{-|
	Creates a structure type whose elements are given by the list
	of types.
-}
llvmMakeStructType :: [FFI.TypeRef] -> Bool -> IO FFI.TypeRef
llvmMakeStructType ts p = withArrayLen ts $
	\len ptr -> FFI.llvmCreateStructType ptr (fromIntegral len) (fromBool p)

{-|
	Creates a named structure type.
-}
llvmMakeNamedStruct :: String -> LLVMContext FFI.TypeRef
llvmMakeNamedStruct xs = get >>=
	\c   -> liftIO $ withCString xs $
	\str -> FFI.llvmCreateNamedStruct c str

{-|
	Updates a struct type (given by the first argument) to use the
	list of types (given by the second arguments) for its elements.
-}
llvmSetStructBody :: FFI.TypeRef -> [FFI.TypeRef] -> Bool -> LLVMContext ()
llvmSetStructBody s ts p = liftIO $ withArrayLen ts $
	\len ptr -> FFI.llvmSetStructBody s ptr (fromIntegral len) (fromBool p)

{--------------------------------------------------------------------------------------------------
                                            End of File                                            
--------------------------------------------------------------------------------------------------}          
