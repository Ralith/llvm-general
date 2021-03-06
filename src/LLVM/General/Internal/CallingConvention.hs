{-# LANGUAGE
  MultiParamTypeClasses,
  TemplateHaskell,
  QuasiQuotes,
  FlexibleInstances
  #-}
module LLVM.General.Internal.CallingConvention where

import LLVM.General.Internal.Coding

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import qualified LLVM.General.AST.CallingConvention as A.CC

instance Monad m => EncodeM m A.CC.CallingConvention FFI.CallConv where
  encodeM cc = return $ 
        case cc of
          A.CC.C -> FFI.callConvC
          A.CC.Fast -> FFI.callConvFast
          A.CC.Cold -> FFI.callConvCold
          A.CC.GHC ->  FFI.CallConv 10
          A.CC.Numbered cc' -> FFI.CallConv (fromIntegral cc')

instance Monad m => DecodeM m A.CC.CallingConvention FFI.CallConv where
  decodeM cc = return $ case cc of
    [FFI.callConvP|C|] -> A.CC.C
    [FFI.callConvP|Fast|] -> A.CC.Fast
    [FFI.callConvP|Cold|] -> A.CC.Cold
    FFI.CallConv 10 -> A.CC.GHC
    FFI.CallConv ci | ci >= 64 -> A.CC.Numbered (fromIntegral ci)
