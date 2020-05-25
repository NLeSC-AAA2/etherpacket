{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.ByteString (ByteString)
import Data.Word (Word16)
import Options.Applicative

import EncodeBits
import Ethernet
import IP
import UDP

import OutputHeader (outputHeader)

data Command = Command
    { udpSpec :: UDPPayload -> UDP
    , toIpSpec :: IPPayload -> IP
    , toEtherSpec :: ByteString -> Ethernet
    , packetLength :: Word16
    , outputPath :: FilePath
    }

udpParser :: Parser (UDPPayload -> UDP)
udpParser = UDP <$> udpPortParser Nothing "source"
                <*> udpPortParser Nothing "dest"

ipParser :: Parser (IPPayload -> IP)
ipParser = IP <$> ipAddressParser Nothing "source"
              <*> ipAddressParser Nothing "dest"

etherParser :: Parser (ByteString -> Ethernet)
etherParser = Ethernet <$> macAddressParser Nothing "source"
                       <*> macAddressParser Nothing "dest"

commandParser :: Parser Command
commandParser = Command <$> udpParser <*> ipParser <*> etherParser
                        <*> udpLengthParser <*> output
  where
    output :: Parser FilePath
    output = outputFilePath <|> pure "/dev/stdout"
      where
        outputFilePath = strOption $ mconcat
            [ metavar "PATH", short 'o', long "output"
            , help "Output file to use.", showDefault ]

    udpLengthParser :: Parser Word16
    udpLengthParser = option auto $ mconcat
        [ metavar "N", short 'l', long "length"
        , help "Payload length in bytes."
        ]

commandlineParser :: ParserInfo Command
commandlineParser = info (commandParser <**> helper) $ mconcat
    [ fullDesc
    , header $ " EtherPacket - a tool for generating packets"
    , progDesc "Generate EtherNet, IP, and UDP packets."
    ]

encode :: EncodeBits a => a -> ByteString
encode = toZeroPaddedByteString . encodeBits

tagFailure :: String -> Maybe a -> Either String a
tagFailure msg Nothing = Left msg
tagFailure _ (Just v) = Right v

main :: IO ()
main = do
    Command{..} <- execParser commandlineParser

    let udpConfig = udpSpec $ UDPLength packetLength
        packet = do
            udpPacket <- tagFailure "UDP" $ mkUDP udpConfig
            ipPacket <- tagFailure "IP" $ mkIP (toIpSpec $ UDP2IP udpPacket)
            tagFailure "Ethernet" $
                encode <$> mkEthernetFrame (toEtherSpec (encode ipPacket))

    case packet of
        Left err -> putStrLn err
        Right p -> outputHeader outputPath p
