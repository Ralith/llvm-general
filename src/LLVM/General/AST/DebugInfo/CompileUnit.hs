module LLVM.General.AST.DebugInfo.CompileUnit where

import Data.Word

import LLVM.General.AST.Operand
import LLVM.General.AST.DebugInfo.Internal
import LLVM.General.AST.DebugInfo.File (File)
import LLVM.General.AST.DebugInfo.Global (Global)
import LLVM.General.AST.DebugInfo.Subprogram (Subprogram)

data CompileUnit = CompileUnit
    { file :: File
    , langID :: Word32
    , compiler :: String
    , optimized :: Bool
    , compileFlags :: String
    , runtimeVersion :: Word32
--    , enums :: [Enum]
--    , retainedTypes :: [RetainedType]
    , subprograms :: [Subprogram]
    , globals :: [Global]
--    , cuImports :: [Import]
    , debugFilename :: String
    }

instance DebugMetadata CompileUnit where
    toMetadata (CompileUnit { file = file
                            , langID = langID
                            , compiler = compiler
                            , optimized = optimized
                            , compileFlags = compileFlags
                            , runtimeVersion = runtimeVersion
--                            , enums = enums
--                            , retainedTypes = retainedTypes
                            , subprograms = subprograms
                            , globals = globals
--                            , cuImports = imports
                            , debugFilename = debugFilename
                            })
        = MetadataNode [ toOp dwTagCompileUnit
                       , toOp file
                       , toOp langID
                       , toOp compiler
                       , toOp optimized
                       , toOp compileFlags
                       , toOp runtimeVersion
                       , nullOp -- , toOp enums
                       , nullOp -- , toOp retainedTypes
                       , toOp subprograms
                       , toOp globals
                       , nullOp --, toOp imports
                       , toOp debugFilename
                       ]

instance Scope CompileUnit where
    fromScope = toMetadata