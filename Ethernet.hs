{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Ethernet where

import Data.Binary.Put (runPut, putWord32le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BS (toStrict)
import Data.Digest.CRC32 (crc32, digest)
import Data.Vector.Unboxed.Sized (Vector)
import Data.Word (Word8, Word16, Word32)

import EncodeBits

newtype MACAddress = MAC (Vector 6 Word8)
    deriving newtype EncodeBitField
    deriving (Show, Read)

instance EncodeBits MACAddress

newtype EtherType = EtherType Word16
    deriving newtype EncodeBitField
    deriving Show

instance EncodeBits EtherType

newtype CRC32 = CRC32 Word32
    deriving (Eq, Bits, Show)

instance EncodeBits CRC32 where
    encodeBits (CRC32 w) = encodeBits . BS.toStrict . runPut . putWord32le $ w

data Ethernet = Ethernet
    { etherDestMac :: MACAddress
    , etherSourceMac :: MACAddress
    , etherPayload :: ByteString
    } deriving (Show)

data EthernetFrame = EtherFrame
    { etherFrameDestMac :: MACAddress
    , etherFrameSourceMac :: MACAddress
    , etherFrameEtherType :: EtherType
    , etherFramePayload :: ByteString
    } deriving (Show)

instance EncodeBits EthernetFrame where
    encodeBits EtherFrame{..} = mconcat
        [ encodeBits etherFrameDestMac
        , encodeBits etherFrameSourceMac
        , encodeBits etherFrameEtherType
        , encodeBits etherFramePayload
        ]

data EthernetPacket = EthernetPacket
    { ethernetFrame :: EthernetFrame
    , ethernetFCS :: CRC32
    } deriving (Show)

instance EncodeBits EthernetPacket where
    encodeBits EthernetPacket{..} = mconcat
        [ encodeBits ethernetFrame
        , encodeBits ethernetFCS
        ]

checkSum :: EthernetFrame -> EthernetPacket
checkSum frame =
    EthernetPacket { ethernetFrame = frame, ethernetFCS = CRC32 checkSumVal }
  where
    byteString = toZeroPaddedByteString . encodeBits $ frame
    checkSumVal = crc32 $ digest byteString

mkEthernet :: Ethernet -> Maybe EthernetPacket
mkEthernet Ethernet{..}
    | dataLength > 1500  = Nothing
    | otherwise = Just . checkSum $ EtherFrame
        { etherFrameDestMac = etherDestMac
        , etherFrameSourceMac = etherSourceMac
        , etherFrameEtherType = EtherType $ 0x0800
        , etherFramePayload = paddedEtherPayload
        }
    where
      dataLength :: Int
      dataLength = BS.length etherPayload

      paddedEtherPayload :: ByteString
      paddedEtherPayload = etherPayload <> BS.replicate (46 - dataLength) 0
