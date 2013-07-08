module LLVM.General.DebugInfo where

import LLVM.General.AST.Operand

import LLVM.General.Internal.DebugInfo
import LLVM.General.DebugInfo.CompileUnit
import LLVM.General.DebugInfo.Location

compileUnitMetadata :: CompileUnit -> MetadataNode
compileUnitMetadata = toMetadata

locationMetadata :: Scope a => Location a -> MetadataNode
locationMetadata = toMetadata