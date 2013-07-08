module LLVM.General.AST.DebugInfo.Location where

import Data.Word

import LLVM.General.AST.Operand
import LLVM.General.AST.DebugInfo.Internal

data Location a = Location { line :: Word32, column :: Word32, scope :: a, originalScope :: Maybe a }

instance Scope a => DebugMetadata (Location a) where
    toMetadata (Location {line = line, column = column, scope = scope, originalScope = origScope}) =
        MetadataNode [ toOp line, toOp column
                     , MetadataNodeOperand (fromScope scope)
                     , maybe nullOp (MetadataNodeOperand . fromScope) origScope
                     ]

