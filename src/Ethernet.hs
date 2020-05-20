{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Ethernet
    ( Ethernet(..)
    , EthernetFrame
    , EthernetPacket
    , MACAddress
    , macAddressParser
    , mkEthernet
    , mkEthernetFrame
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import qualified Data.Attoparsec.Text as Atto
import Data.Binary.Put (runPut, putWord32le)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BS (toStrict)
import Data.Digest.CRC32 (crc32, digest)
import qualified Data.Text as T
import Data.Vector.Unboxed.Sized (Vector)
import qualified Data.Vector.Unboxed.Sized as Sized
import Data.Word (Word8, Word16, Word32)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ReadM)
import qualified Options.Applicative as Optparse

import EncodeBits

newtype MACAddress = MAC (Vector 6 Word8)
    deriving newtype EncodeBitField
    deriving (Show, Read)

instance EncodeBits MACAddress

macAddressParser :: Maybe Char -> String -> Parser MACAddress
macAddressParser shortOption prefix =
  Optparse.option (attoParse macAddress) $ mconcat
    [ Optparse.metavar "IP", foldMap Optparse.short shortOption
    , Optparse.long (prefix <> "-mac"), Optparse.help "MAC address to use."
    ]
  where
    attoParse :: Atto.Parser a -> ReadM a
    attoParse p = Optparse.eitherReader $
        Atto.parseOnly (p <* Atto.endOfInput) . T.pack

    macAddress :: Atto.Parser MACAddress
    macAddress = MAC <$> Sized.replicateM (hexPair <* separator)

    separator :: Atto.Parser ()
    separator = void (Atto.char ':') <|> (Atto.atEnd >>= guard)

    hexPair :: Atto.Parser Word8
    hexPair = do
        v <- Atto.hexadecimal :: Atto.Parser Natural
        guard $ v <= fromIntegral (maxBound :: Word8)
        return $ fromIntegral v

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

rawMkEthernet :: Bool -> Ethernet -> Maybe EthernetFrame
rawMkEthernet pad Ethernet{..}
    | dataLength > 1500  = Nothing
    | otherwise = Just EtherFrame
        { etherFrameDestMac = etherDestMac
        , etherFrameSourceMac = etherSourceMac
        , etherFrameEtherType = EtherType $ 0x0800
        , etherFramePayload = paddedEtherPayload
        }
    where
      dataLength :: Int
      dataLength = BS.length etherPayload

      paddedEtherPayload :: ByteString
      paddedEtherPayload
        | pad = etherPayload <> BS.replicate (46 - dataLength) 0
        | otherwise = etherPayload


mkEthernet :: Ethernet -> Maybe EthernetPacket
mkEthernet ether = checkSum <$> rawMkEthernet True ether

mkEthernetFrame :: Ethernet -> Maybe EthernetFrame
mkEthernetFrame ether = rawMkEthernet False ether
