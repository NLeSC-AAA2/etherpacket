{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Bits ((.|.), shiftL)
import Control.Monad (forM_, guard, void)
import qualified Data.Attoparsec.Text as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Vector.Unboxed.Sized as Sized
import Data.Word (Word8, Word16, Word32)
import Numeric.Natural (Natural)
import Options.Applicative
import System.IO (IOMode(WriteMode), hPutStrLn, withFile)
import Text.Show.Pretty

import EncodeBits
import Ethernet
import IP
import UDP

attoParse :: Atto.Parser a -> ReadM a
attoParse p = eitherReader $ Atto.parseOnly (p <* Atto.endOfInput) . T.pack

payloadParser :: Parser (IO ByteString)
payloadParser = BS.readFile <$> payloadFile <|> args
  where
    payloadFile :: Parser FilePath
    payloadFile = strOption $ mconcat
        [ metavar "FILE", short 'p', long "payload"
        , help "File to read payload from."
        ]

    args :: Parser (IO ByteString)
    args = return . BS.unwords <$> some arg

    arg :: Parser ByteString
    arg = strArgument $ mconcat
        [ metavar "PAYLOAD" ]

udpParser :: Parser (ByteString -> UDP)
udpParser = UDP <$> port "source" <*> port "dest"
  where
    port :: String -> Parser Word16
    port "" = empty
    port name@(c:_) = option auto $ mconcat
        [ metavar "PORT", short c, long name, help "Port to use." ]

ipParser :: Parser (ByteString -> IP)
ipParser = IP <$> ip "source" <*> ip "dest"
  where
    ip :: String -> Parser Word32
    ip "" = empty
    ip name@(c:_) = option (attoParse ipAddress) $ mconcat
        [ metavar "IP", short c, long name, help "IP address to use." ]

    ipAddress :: Atto.Parser Word32
    ipAddress = do
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

etherParser :: Parser (ByteString -> Ethernet)
etherParser = Ethernet <$> mac "source" <*> mac "dest"
  where
    mac :: String -> Parser MACAddress
    mac "" = empty
    mac name@(c:_) = option (attoParse macAddress) $ mconcat
        [ metavar "MAC", short c, long name, help "MAC address to use." ]

    macAddress :: Atto.Parser MACAddress
    macAddress = MAC <$> Sized.replicateM (hexPair <* separator)

    separator :: Atto.Parser ()
    separator = void (Atto.char ':') <|> (Atto.atEnd >>= guard)

    hexPair :: Atto.Parser Word8
    hexPair = do
        v <- Atto.hexadecimal :: Atto.Parser Natural
        guard $ v <= fromIntegral (maxBound :: Word8)
        return $ fromIntegral v

data PacketSpec
    = Udp UDP
    | Ip IP
    | Ether Ethernet
    deriving (Show)

data Command = Command
    { toPacketSpec :: ByteString -> PacketSpec
    , humanReadable :: Bool
    , outputPath :: FilePath
    , payload :: IO ByteString
    }

commandParser :: Parser Command
commandParser =
    Command <$> subcommands <*> readable <*> output <*> payloadParser
  where
    readable :: Parser Bool
    readable = flag False True $ mconcat
        [ long "human-readable"
        , help "Outut human readable rendering of data"
        ]

    output :: Parser FilePath
    output = outputFilePath <|> pure "/dev/stdout"
      where
        outputFilePath = strOption $ mconcat
            [ metavar "PATH", short 'o', long "output"
            , help "Output file to use.", showDefault ]

    subCommand :: String-> String -> String -> Parser a -> Mod CommandFields a
    subCommand cmd hdr desc parser = command cmd . info parser $ mconcat
        [ fullDesc
        , header $ cmd ++ " - " ++ hdr
        , progDesc desc
        ]

    subcommands = hsubparser $ mconcat
        [ subCommand "ether" "generate an EtherNet packet"
            "Generate an EtherNet packet" $ (Ether .) <$> etherParser
        , subCommand "ip" "generate an IP packet"
            "Generate an IP packet" $ (Ip .) <$> ipParser
        , subCommand "udp" "generate an UDP packet"
            "Generate an UDP packet" $ (Udp .) <$> udpParser
        ]

commandlineParser :: ParserInfo Command
commandlineParser = info (commandParser <**> helper) $ mconcat
    [ fullDesc
    , header $ " EtherPacket - a tool for generating packets"
    , progDesc "Generate EtherNet, IP, and UDP packets."
    ]

outputPacket :: (EncodeBits a, Show a) => Bool -> FilePath -> a -> IO ()
outputPacket humanReadable path packet = withFile path WriteMode $ \hnd -> do
    if not humanReadable
       then BS.hPut hnd byteString
       else do
        hPutStrLn hnd $ ppShow packet
        hPutStrLn hnd ""
        hPutStrLn hnd $ show packetBits
  where
    byteString = toZeroPaddedByteString packetBits
    packetBits = encodeBits packet

main :: IO ()
main = do
    Command{..} <- execParser commandlineParser
    let dumpPacket :: (EncodeBits a, Show a) => a -> IO ()
        dumpPacket = outputPacket humanReadable outputPath

    packetSpec <- toPacketSpec <$> payload
    print packetSpec
    putStrLn ""
    case packetSpec of
        Udp udp -> forM_ (mkUDP udp) dumpPacket
        Ip ip -> forM_ (mkIP ip) dumpPacket
        Ether ether -> forM_ (mkEthernet ether) dumpPacket
