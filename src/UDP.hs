{-# LANGUAGE RecordWildCards #-}
module UDP (UDP(..), UDPPacket, UDPPort, mkUDP, udpPortParser) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Options.Applicative (Parser)
import qualified Options.Applicative as Optparse

import EncodeBits

newtype UDPPort = UDPPort Word16
    deriving (Show)

instance EncodeBits UDPPort where
    encodeBits (UDPPort p) = encodeBits p

udpPortParser :: Maybe Char -> String -> Parser UDPPort
udpPortParser shortOption prefix =
  Optparse.option (UDPPort <$> Optparse.auto) $ mconcat
        [ Optparse.metavar "PORT", foldMap Optparse.short shortOption
        , Optparse.long (prefix <> "-port")
        , Optparse.help "Port to use."
        ]

data UDP = UDP
    { udpSourcePort :: UDPPort
    , udpDestPort :: UDPPort
    , udpPayload :: ByteString
    } deriving (Show)

data UDPPacket = UDPPacket
    { udpPacketSourcePort :: UDPPort
    , udpPacketDestPort :: UDPPort
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
