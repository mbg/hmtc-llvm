{--------------------------------------------------------------------------------------------------
                                     LLVM Bindings for Haskell                                     
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

{-| 
	This module contains high-level bindings for the LLVM core functions.
-}
module LLVM.Core (
    Value
) where

{----------------------------------------------------------------------}
{-- Module Imports                                                    -}
{----------------------------------------------------------------------}

import qualified LLVM.Base.Core as Core

{----------------------------------------------------------------------}
{-- Types                                                             -}
{----------------------------------------------------------------------}

class Value a where
    unpack :: a -> Core.ValueRef
    pack   :: Core.ValueRef -> a

newtype Function = F Core.ValueRef 

instance Value Function where
    unpack (F v) = v
    pack         = F
    
    