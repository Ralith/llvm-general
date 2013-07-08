module LLVM.General.DebugInfo.File where

import LLVM.General.AST.Operand
import LLVM.General.Internal.DebugInfo

data File = File { name :: FilePath, directory :: FilePath }

instance DebugMetadata File where
    toMetadata (File {name = name, directory = directory}) =
        MetadataNode [ toOp name, toOp directory ]
