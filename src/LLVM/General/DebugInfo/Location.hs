module LLVM.General.DebugInfo.Location where

import Data.Word

import LLVM.General.AST.Operand
import LLVM.General.Internal.DebugInfo

data Location a = Location { line :: Word, column :: Word, scope :: a, originalScope :: Maybe a }

instance Scope a => DebugMetadata (Location a) where
    toMetadata (Location {line = line, column = column, scope = scope, originalScope = origScope}) =
        MetadataNode [ toOp (fromIntegral line :: Word32), toOp (fromIntegral column :: Word32)
                     , MetadataNodeOperand (fromScope scope)
                     , maybe nullOp (MetadataNodeOperand . fromScope) origScope
                     ]

