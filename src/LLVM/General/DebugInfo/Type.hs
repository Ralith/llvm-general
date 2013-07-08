module LLVM.General.DebugInfo.Type where

import Data.Word

import LLVM.General.AST.Operand
import LLVM.General.Internal.DebugInfo

data Type = BasicType { name :: String
                      , size :: Word
                      , align :: Word
                      , encoding :: Encoding
                      }

instance DebugMetadata Type where
  toMetadata (BasicType { name = name, size = size, align = align, encoding = encoding }) =
      MetadataNode [ toOp dwTagBaseType
                   , nullOp, nullOp
                   , toOp name
                   , toOp (0 :: Word32)
                   , toOp (fromIntegral size :: Word64)
                   , toOp (fromIntegral align :: Word64)
                   , toOp (0 :: Word32)
                   , toOp (0 :: Word32)
                   , toOp encoding
                   ]

data Encoding = Address | Boolean | ComplexFloat | Float | Signed | SignedChar
              | Unsigned | UnsignedChar | ImaginaryFloat | PackedDecimal
              | NumericString | EditedString | SignedFixed | UnsignedFixed
              | DecimalFloat | UTF | User Word32
  deriving (Eq, Show)

instance DebugMetadataOp Encoding where
  toOp Address = toOp (0x01 :: Word32)
  toOp Boolean = toOp (0x02 :: Word32)
  toOp ComplexFloat = toOp (0x03 :: Word32)
  toOp Float = toOp (0x04 :: Word32)
  toOp Signed = toOp (0x05 :: Word32)
  toOp SignedChar = toOp (0x06 :: Word32)
  toOp Unsigned = toOp (0x07 :: Word32)
  toOp UnsignedChar = toOp (0x08 :: Word32)
  toOp ImaginaryFloat = toOp (0x09 :: Word32)
  toOp PackedDecimal = toOp (0x0a :: Word32)
  toOp NumericString = toOp (0x0b :: Word32)
  toOp EditedString = toOp (0x0c :: Word32)
  toOp SignedFixed = toOp (0x0d :: Word32)
  toOp UnsignedFixed = toOp (0x0e :: Word32)
  toOp DecimalFloat = toOp (0x0f :: Word32)
  toOp (User n) = toOp (0x80 + fromIntegral n :: Word32)