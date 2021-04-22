{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module EncodeBits
    ( Bits
    , EncodeBits(..)
    , EncodeBitField(..)
    , toZeroPaddedByteString
    , toZeroPaddedVector
    ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.Bit (Bit(..))
import Data.Bits (Bits(setBit, testBit), finiteBitSize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Finite (getFinite)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import qualified Data.Vector.Generic as V hiding (new)
import qualified Data.Vector.Generic.Mutable as V
    (new, unsafeModify, unsafeWrite)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VS (STVector)
import qualified Data.Vector.Unboxed as VU (Vector, Unbox, replicate)
import qualified Data.Vector.Unboxed.Mutable as VU (STVector)
import Data.Vector.Unboxed.Sized (Vector, fromSized)
import qualified Data.Vector.Unboxed.Sized as Sized
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr, plusForeignPtr)
import Foreign.Storable (Storable(sizeOf))
import GHC.TypeLits

newtype BitString = Bits (VU.Vector Bit) deriving (Eq)

instance Semigroup BitString where
    Bits b1 <> Bits b2 = Bits $ b1 <> b2

instance Monoid BitString where
    mempty = Bits mempty

instance Show BitString where
    show (Bits v) = formatLines spacedNibbles . map fromBit $ V.toList v
      where
        formatLines :: (String -> String) -> String -> String
        formatLines f = unlines . map f . chunksOf 32

        spacedNibbles :: String -> String
        spacedNibbles = intercalate " " . chunksOf 4

        fromBit :: Bit -> Char
        fromBit (Bit True) = '1'
        fromBit (Bit False) = '0'

class EncodeBits a where
    encodeBits :: a -> BitString
    default encodeBits :: (KnownNat (BitSizeField a), EncodeBitField a)
                       => a -> BitString
    encodeBits v = Bits . fromSized $ encodeBitField v

class EncodeBitField a where
    type BitSizeField a :: Nat

    encodeBitField :: a -> Vector (BitSizeField a) Bit
    default encodeBitField :: (Bits a, KnownNat (BitSizeField a))
                           => a -> Vector (BitSizeField a) Bit
    encodeBitField v = result
      where
        result = Sized.generate (Bit . testBit v . (maxIdx-) . fromFinite)
        fromFinite = fromIntegral . getFinite
        maxIdx = Sized.length result - 1

toZeroPaddedVector :: forall a . Storable a => BitString -> VS.Vector a
toZeroPaddedVector (Bits bits) = VS.unsafeCast $ V.create $ do
    bytes <- V.new (V.length paddedVector `quot` 8)
    V.imapM_ (toggleBit bytes) paddedVector
    return bytes
  where
    toggleBit :: VS.STVector s Word8 -> Int -> Bit -> ST s ()
    toggleBit _ _ (Bit False) = return ()
    toggleBit vec i (Bit True) = do
        V.unsafeModify vec (`setBit` (7 - bit)) idx
      where
        (idx, bit) = i `quotRem` 8

    elemBitSize :: Int
    elemBitSize = 8 * sizeOf (undefined :: a)

    bitsRemaining :: Int
    bitsRemaining = V.length bits `rem` elemBitSize

    padLength :: Int
    padLength
        | bitsRemaining == 0 = 0
        | otherwise = elemBitSize - bitsRemaining

    paddedVector :: VU.Vector Bit
    paddedVector = bits <> VU.replicate padLength (Bit False)

toZeroPaddedByteString :: BitString -> ByteString
toZeroPaddedByteString bits = BS.fromForeignPtr fptr 0 len
  where
    (fptr, len) = VS.unsafeToForeignPtr0 . toZeroPaddedVector $ bits

instance EncodeBits Bit
instance EncodeBitField Bit where
    type BitSizeField Bit = 1

instance EncodeBits Int8
instance EncodeBitField Int8 where
    type BitSizeField Int8 = 8

instance EncodeBits Word8
instance EncodeBitField Word8 where
    type BitSizeField Word8 = 8

instance EncodeBits Int16
instance EncodeBitField Int16 where
    type BitSizeField Int16 = 16

instance EncodeBits Word16
instance EncodeBitField Word16 where
    type BitSizeField Word16 = 16

instance EncodeBits Int32
instance EncodeBitField Int32 where
    type BitSizeField Int32 = 32

instance EncodeBits Word32
instance EncodeBitField Word32 where
    type BitSizeField Word32 = 32

instance EncodeBits Int64
instance EncodeBitField Int64 where
    type BitSizeField Int64 = 64

instance EncodeBits Word64
instance EncodeBitField Word64 where
    type BitSizeField Word64 = 64

instance (VU.Unbox a, EncodeBitField a, KnownNat (n * BitSizeField a))
    => EncodeBits (Vector n a)

instance (VU.Unbox a, EncodeBitField a) => EncodeBitField (Vector n a) where
    type BitSizeField (Vector n a) = n * BitSizeField a

    encodeBitField v = Sized.concatMap encodeBitField v

instance EncodeBits ByteString where
    encodeBits inputBS = Bits $ V.create $ do
        bits <- V.new (inputLength * 8)
        V.imapM_ (toggleBit bits) byteVec
        return bits
      where
        inputLength :: Int
        inputLength = BS.length inputBS

        byteVec :: VS.Vector Word8
        byteVec = VS.unsafeFromForeignPtr0 (plusForeignPtr bsPtr bsOff) bsLen

        bsPtr :: ForeignPtr Word8
        bsOff, bsLen :: Int
        (bsPtr, bsOff, bsLen) = BS.toForeignPtr inputBS

        toggleBit :: VU.STVector s Bit -> Int -> Word8 -> ST s ()
        toggleBit vec idx w = forM_ [0 .. (finiteBitSize w - 1)] $ \bit ->
            V.unsafeWrite vec (offset + bit) (Bit (w `testBit` (7 - bit)))
          where
            offset = 8 * idx
