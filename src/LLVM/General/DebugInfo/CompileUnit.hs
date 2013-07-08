module LLVM.General.DebugInfo.CompileUnit where

import Data.Word

import LLVM.General.AST.Operand
import LLVM.General.Internal.DebugInfo
import LLVM.General.DebugInfo.File (File)
import LLVM.General.DebugInfo.Global (Global)
import LLVM.General.DebugInfo.Subprogram (Subprogram)

data CompileUnit = CompileUnit
    { file :: File
    , langID :: Word
    , compiler :: String
    , optimized :: Bool
    , compileFlags :: String
    , runtimeVersion :: Word
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
                       , toOp (fromIntegral langID :: Word32)
                       , toOp compiler
                       , toOp optimized
                       , toOp compileFlags
                       , toOp (fromIntegral runtimeVersion :: Word32)
                       , nullOp -- , toOp enums
                       , nullOp -- , toOp retainedTypes
                       , toOp subprograms
                       , toOp globals
                       , nullOp --, toOp imports
                       , toOp debugFilename
                       ]

instance Scope CompileUnit where
    fromScope = toMetadata