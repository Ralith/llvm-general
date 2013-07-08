module LLVM.General.AST.DebugInfo where

import LLVM.General.AST.Operand

import LLVM.General.AST.DebugInfo.Internal
import LLVM.General.AST.DebugInfo.Scope

compileUnitMetadata :: CompileUnit -> MetadataNode
compileUnitMetadata = toMetadata

locationMetadata :: Location -> MetadataNode
locationMetadata = toMetadata