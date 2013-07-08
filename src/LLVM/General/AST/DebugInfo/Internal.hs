{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
module LLVM.General.AST.DebugInfo.Internal where

import LLVM.General.AST.Operand
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.Type hiding (Type)

import Foreign.C.Types
import Data.Word
import Data.Bits

-- llvm/Support/Dwarf.h
llvmDebugVersion :: CUInt
llvmDebugVersion = 12 `shiftL` 16

newtype DwarfTag = DwarfTag CUInt
dwTagCompileUnit = DwarfTag 0x11
dwTagBaseType = DwarfTag 0x24
dwTagEnumerator = DwarfTag 0x28
dwTagSubprogram = DwarfTag 0x2e
dwTagVariable = DwarfTag 0x34

nullOp :: Operand
nullOp = ConstantOperand (C.Null (IntegerType 32))


class DebugMetadata a where
    toMetadata :: a -> MetadataNode

class DebugMetadataOp a where
    toOp :: a -> Operand

instance DebugMetadataOp String where
    toOp = MetadataStringOperand

instance DebugMetadataOp Bool where
    toOp False = ConstantOperand (C.Int 1 0)
    toOp True = ConstantOperand (C.Int 1 1)

instance DebugMetadataOp CUInt where
    toOp = ConstantOperand . C.Int 32 . fromIntegral

instance DebugMetadataOp Word32 where
    toOp = ConstantOperand . C.Int 32 . fromIntegral

instance DebugMetadataOp Word64 where
    toOp = ConstantOperand . C.Int 64 . fromIntegral

instance DebugMetadataOp DwarfTag where
    toOp (DwarfTag t) = toOp (t .|. llvmDebugVersion)

instance DebugMetadata a => DebugMetadataOp a where
    toOp = MetadataNodeOperand . toMetadata

instance DebugMetadataOp a => DebugMetadataOp (Maybe a) where
    toOp Nothing = nullOp
    toOp (Just x) = toOp x

instance DebugMetadataOp a => DebugMetadataOp [a] where
    toOp xs = MetadataNodeOperand . MetadataNode $ map toOp xs

