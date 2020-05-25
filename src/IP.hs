{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module IP
    ( IP(..)
    , IPAddress
    , IPPacket
    , IPPayload(..)
    , ipAddressParser
    , mkIP
    ) where

import Control.Monad (guard)
import qualified Data.Attoparsec.Text as Atto
import Data.Bit (Bit(..))
import Data.Bits (Bits, (.&.), (.|.), complement, rotate, shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed.Sized as Sized
import Data.Word (Word8, Word16, Word32)
import Numeric.Natural (Natural)
import Options.Applicative (Parser, ReadM)
import qualified Options.Applicative as Optparse

import EncodeBits
import UDP

newtype Version = Version Word8
    deriving (Eq, Bits, Show)

instance EncodeBits Version
instance EncodeBitField Version where
    type BitSizeField Version = 4

newtype IHL = IHL Word8
    deriving (Eq, Bits, Show)

instance EncodeBits IHL
instance EncodeBitField IHL where
    type BitSizeField IHL = 4

data DSCP = DSCP deriving (Show)

instance EncodeBits DSCP
instance EncodeBitField DSCP where
    type BitSizeField DSCP = 6

    encodeBitField DSCP = Sized.replicate (Bit False)

data ECN = ECN deriving (Show)

instance EncodeBits ECN
instance EncodeBitField ECN where
    type BitSizeField ECN = 2

    encodeBitField ECN = Sized.replicate (Bit False)

data Flag = DontFragment | MoreFragments deriving (Eq, Ord, Show)

instance EncodeBits Flag
instance EncodeBitField Flag where
    type BitSizeField Flag = 3

    encodeBitField flag = case flag of
        DontFragment -> setFlag 1
        MoreFragments -> setFlag 2
      where
        zeroVector = Sized.replicate (Bit False)

        setFlag n = Sized.update zeroVector $ Sized.singleton (n, Bit True)

newtype Flags = Flags (Set Flag) deriving (Show)

instance EncodeBits Flags
instance EncodeBitField Flags where
    type BitSizeField Flags = 3

    encodeBitField (Flags flags) = foldFlags . S.map encodeBitField $ flags
      where
        zeroVector = Sized.replicate (Bit False)
        foldFlags = S.foldr' (Sized.zipWith (.|.)) zeroVector

newtype FragmentOffset = FragmentOffset Word16
    deriving (Eq, Bits, Show)

instance EncodeBits FragmentOffset
instance EncodeBitField FragmentOffset where
    type BitSizeField FragmentOffset = 13

data IPAddress = IPAddress Word32
    deriving (Show)

instance EncodeBits IPAddress where
    encodeBits (IPAddress ip) = encodeBits ip

ipAddressParser :: Maybe Char -> String -> Parser IPAddress
ipAddressParser shortOption prefix =
  Optparse.option (attoParse ipAddress) $ mconcat
    [ Optparse.metavar "IP", foldMap Optparse.short shortOption
    , Optparse.long (prefix <> "-ip"), Optparse.help "IP address to use."
    ]
  where
    attoParse :: Atto.Parser a -> ReadM a
    attoParse p = Optparse.eitherReader $
        Atto.parseOnly (p <* Atto.endOfInput) . T.pack

    ipAddress :: Atto.Parser IPAddress
    ipAddress = IPAddress <$> do
        octet1 <- octet Nothing <* Atto.char '.'
        octet2 <- octet (Just octet1) <* Atto.char '.'
        octet3 <- octet (Just octet2) <* Atto.char '.'
        octet (Just octet3)
      where
        octet :: Maybe Word32 -> Atto.Parser Word32
        octet mOctets = do
            v <- Atto.decimal :: Atto.Parser Word32
            guard $ v < fromIntegral (maxBound :: Word8)
            return $ prevOctets .|. v
          where
            prevOctets = case mOctets of
                Nothing -> 0
                Just v -> shiftL v 8

data IPPayload = UDP2IP UDPPacket | IPPayload ByteString deriving Show

data IP = IP
     { ipSourceIP :: IPAddress
     , ipDestIP :: IPAddress
     , ipPayload :: IPPayload
     } deriving (Show)

data IPPacket = IPPacket
    { ipPacketVersion :: Version
    , ipPacketIHL :: IHL
    , ipPacketDSCP :: DSCP
    , ipPacketECN :: ECN
    , ipPacketLength :: Word16
    , ipPacketIdentification :: Word16
    , ipPacketFlag :: Flags
    , ipPacketFragmentOffset :: FragmentOffset
    , ipPacketTTL :: Word8
    , ipPacketProtocol :: Word8
    , ipPacketHeaderChecksum :: Word16
    , ipPacketSourceIP :: IPAddress
    , ipPacketDestIP :: IPAddress
    , ipPacketOptions :: ByteString
    , ipPacketPayload :: ByteString
    } deriving (Show)

instance EncodeBits IPPacket where
    encodeBits IPPacket{..} = mconcat
        [ encodeBits ipPacketVersion
        , encodeBits ipPacketIHL
        , encodeBits ipPacketDSCP
        , encodeBits ipPacketECN
        , encodeBits ipPacketLength
        , encodeBits ipPacketIdentification
        , encodeBits ipPacketFlag
        , encodeBits ipPacketFragmentOffset
        , encodeBits ipPacketTTL
        , encodeBits ipPacketProtocol
        , encodeBits ipPacketHeaderChecksum
        , encodeBits ipPacketSourceIP
        , encodeBits ipPacketDestIP
        , encodeBits ipPacketOptions
        , encodeBits ipPacketPayload
        ]

checkSum :: IPPacket -> IPPacket
checkSum packet@IPPacket{} = packet { ipPacketHeaderChecksum = checkSumVal }
  where
    checkSumVal :: Word16
    checkSumVal = (`rotate` 8) . complement . carryOver $ result
      where
        result = VS.foldl' add 0 headerWords

        add :: Natural -> Word16 -> Natural
        add r v = r + fromIntegral v

        carryOver :: Natural -> Word16
        carryOver n
            | n <= maxVal = fromIntegral n
            | otherwise = carryOver $ (mask .&. n) + shiftR n 16

        mask :: Natural
        mask = fromIntegral (complement 0 :: Word16)

        maxVal :: Natural
        maxVal = fromIntegral (maxBound :: Word16)

    headerWords :: Vector Word16
    headerWords = toZeroPaddedVector $ encodeBits headerOnly

    headerOnly = packet
      { ipPacketHeaderChecksum = 0
      , ipPacketPayload = BS.empty
      }

mkIP :: IP -> Maybe IPPacket
mkIP IP{..}
    | dataLength > fromIntegral maxLength = Nothing
    | otherwise = Just . checkSum $ IPPacket
        { ipPacketVersion = Version 4
        , ipPacketIHL = IHL headerLength
        , ipPacketDSCP = DSCP
        , ipPacketECN = ECN
        , ipPacketLength = fromIntegral $ dataLength + 4 * headerLength
        , ipPacketIdentification = 0
        , ipPacketFlag = Flags $ S.fromList [DontFragment]
        , ipPacketFragmentOffset = FragmentOffset $ 0
        , ipPacketTTL = 64
        , ipPacketProtocol = 17
        , ipPacketHeaderChecksum = 0
        , ipPacketSourceIP = ipSourceIP
        , ipPacketDestIP = ipDestIP
        , ipPacketOptions = BS.empty
        , ipPacketPayload = payloadData
        }
  where
    headerLength, optionLength :: Integral a => a
    optionLength = 0
    headerLength = 5 + optionLength

    maxLength :: Word16
    maxLength = maxBound - (4 * headerLength)

    dataLength :: Int
    payloadData :: ByteString
    (dataLength, payloadData) = case ipPayload of
        UDP2IP udpPacket -> ( fromIntegral (udpPacketLength udpPacket)
                            , toZeroPaddedByteString (encodeBits udpPacket)
                            )
        IPPayload bs -> (BS.length bs, bs)
