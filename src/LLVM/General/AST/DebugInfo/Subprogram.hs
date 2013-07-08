module LLVM.General.AST.DebugInfo.Subprogram where

import Data.Word

import LLVM.General.AST.Name
import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.DebugInfo.Internal
import LLVM.General.AST.DebugInfo.File (File)
import LLVM.General.AST.DebugInfo.Type (Type)

data Subprogram = Subprogram { name :: String
                             , linkageName :: String
                             , spFile :: File
                             , spLine :: Word32
                             , ty :: Type
                             , isLocal :: Bool
                             , isDefinition :: Bool
                             , isOptimized :: Bool
                             , llvmName :: Name
                             }

instance DebugMetadata Subprogram where
    toMetadata (Subprogram { name = name, linkageName = linkageName, spFile = spFile
                           , spLine = spLine, ty = ty, isLocal = isLocal
                           , isDefinition = isDefinition, isOptimized = isOptimized
                           , llvmName = llvmName }) =
        MetadataNode [ toOp dwTagSubprogram
                     , toOp spFile
                     , nullOp
                     , toOp name
                     , toOp name
                     , toOp linkageName
                     , toOp spLine
                     , toOp ty
                     , toOp isLocal
                     , toOp isDefinition
                     , toOp (0 :: Word32), toOp (0 :: Word32)
                     , nullOp, nullOp
                     , toOp isOptimized
                     , ConstantOperand . C.GlobalReference $ llvmName
                     , nullOp
                     , nullOp
                     , nullOp
                     , toOp spLine
                     ]

instance Scope Subprogram where
    fromScope = toMetadata