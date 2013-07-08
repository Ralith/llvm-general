module LLVM.General.AST.DebugInfo where

import LLVM.General.AST.Operand

import LLVM.General.AST.DebugInfo.Internal
import LLVM.General.AST.DebugInfo.CompileUnit
import LLVM.General.AST.DebugInfo.Location

compileUnitMetadata :: CompileUnit -> MetadataNode
compileUnitMetadata = toMetadata

locationMetadata :: Scope a => Location a -> MetadataNode
locationMetadata = toMetadata