module LLVM.General.AST.DebugInfo.File where

import LLVM.General.AST.Operand
import LLVM.General.AST.DebugInfo.Internal

data File = File { name :: FilePath, directory :: FilePath }

instance DebugMetadata File where
    toMetadata (File {name = name, directory = directory}) =
        MetadataNode [ toOp name, toOp directory ]
