{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Options.Applicative
import System.IO (IOMode(WriteMode), hPutStrLn, withFile)
import Text.Show.Pretty

import EncodeBits
import Ethernet
import IP
import UDP

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

udpParser :: Parser (UDPPayload -> UDP)
udpParser = UDP <$> udpPortParser (Just 's') "source"
                <*> udpPortParser (Just 'd') "dest"

ipParser :: Parser (IPPayload -> IP)
ipParser = IP <$> ipAddressParser (Just 's') "source"
              <*> ipAddressParser (Just 'd') "dest"

etherParser :: Parser (ByteString -> Ethernet)
etherParser = Ethernet <$> macAddressParser (Just 's') "source"
                       <*> macAddressParser (Just 'd') "dest"

data PacketSpec
    = Udp UDP
    | Ip IP
    | Ether Ethernet
    deriving (Show)

mkUdpSpec :: (UDPPayload -> UDP) -> ByteString -> PacketSpec
mkUdpSpec f bs = Udp . f $ UDPPayload bs

mkIpSpec :: (IPPayload -> IP) -> ByteString -> PacketSpec
mkIpSpec f bs = Ip . f $ IPPayload bs

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
            "Generate an IP packet" $ mkIpSpec <$> ipParser
        , subCommand "udp" "generate an UDP packet"
            "Generate an UDP packet" $ mkUdpSpec <$> udpParser
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
