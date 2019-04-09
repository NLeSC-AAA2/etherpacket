{-# LANGUAGE RecordWildCards #-}
module UDP (UDP(..), UDPPacket, mkUDP) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)

import EncodeBits

data UDP = UDP
    { udpSourcePort :: Word16
    , udpDestPort :: Word16
    , udpPayload :: ByteString
    } deriving (Show)

data UDPPacket = UDPPacket
    { udpPacketSourcePort :: Word16
    , udpPacketDestPort :: Word16
    , udpPacketLength :: Word16
    , udpPacketChecksum :: Word16
    , udpPacketPayload :: ByteString
    } deriving (Show)

instance EncodeBits UDPPacket where
    encodeBits UDPPacket{..} = mconcat
        [ encodeBits udpPacketSourcePort
        , encodeBits udpPacketDestPort
        , encodeBits udpPacketLength
        , encodeBits udpPacketChecksum
        , encodeBits udpPacketPayload
        ]

mkUDP :: UDP -> Maybe UDPPacket
mkUDP UDP{..}
    | dataLength > fromIntegral maxLength = Nothing
    | otherwise = Just UDPPacket
        { udpPacketSourcePort = udpSourcePort
        , udpPacketDestPort = udpDestPort
        , udpPacketLength = fromIntegral $ 8 + dataLength
        , udpPacketChecksum = 0
        , udpPacketPayload = udpPayload
        }
    where
      maxLength :: Word16
      maxLength = maxBound - 8

      dataLength :: Int
      dataLength = BS.length udpPayload
