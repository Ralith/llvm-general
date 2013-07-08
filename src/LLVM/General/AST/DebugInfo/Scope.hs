module LLVM.General.AST.DebugInfo.Scope where

import Data.Word

import LLVM.General.AST.Name
import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.DebugInfo.Internal
import LLVM.General.AST.DebugInfo.File (File)
import LLVM.General.AST.DebugInfo.Global (Global)
import LLVM.General.AST.DebugInfo.Type (Type)

data Scope = CompileUnitScope CompileUnit | SubprogramScope Subprogram

instance DebugMetadata Scope where
    toMetadata (CompileUnitScope cu) = toMetadata cu
    toMetadata (SubprogramScope sp) = toMetadata sp

data Location = Location { line :: Word32, column :: Word32, scope :: Scope, originalScope :: Maybe Scope }

instance DebugMetadata Location where
    toMetadata (Location {line = line, column = column, scope = scope, originalScope = origScope}) =
        MetadataNode [ toOp line, toOp column
                     , toOp scope
                     , toOp origScope
                     ]

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