module LLVM.General.AST.DebugInfo.Global where

import LLVM.General.AST.Name
import LLVM.General.AST.Operand
import LLVM.General.AST.Constant as C
import LLVM.General.AST.DebugInfo.Internal
import qualified LLVM.General.AST.DebugInfo.File as F
import qualified LLVM.General.AST.DebugInfo.Type as T

import Data.Word

data Global = Global { name :: String, linkageName :: String, file :: F.File
                     , line :: Word32, ty :: T.Type, isLocal :: Bool, llvmName :: Name }

instance DebugMetadata Global where
    toMetadata (Global {name = name, linkageName = linkName, file = file, line = line, ty = ty
                       , isLocal = isLocal, llvmName = llvmName }) =
        MetadataNode [ toOp dwTagVariable
                     , nullOp, nullOp
                     , toOp name, toOp name, toOp linkName
                     , toOp file, toOp line, toOp ty, toOp isLocal
                     , ConstantOperand . C.GlobalReference $ llvmName]
