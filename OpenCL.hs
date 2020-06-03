{-# LANGUAGE OverloadedStrings #-}
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
    , payloadType :: (String, Word16)
    , payloadLength :: Word16
    , outputPath :: FilePath
    }

udpParser :: Parser (UDPPayload -> UDP)
udpParser = UDP <$> udpPortParser Nothing "source" (Just 53118)
                <*> udpPortParser Nothing "dest" (Just 4440)

ipParser :: Parser (IPPayload -> IP)
ipParser = IP <$> ipAddressParser Nothing "source" (Just "10.196.248.254")
              <*> ipAddressParser Nothing "dest" (Just "10.196.248.2")

etherParser :: Parser (ByteString -> Ethernet)
etherParser = Ethernet <$> macAddressParser Nothing "source" (Just "F4:52:14:94:DC:C1")
                       <*> macAddressParser Nothing "dest" (Just "00:07:43:3B:F6:40")

commandParser :: Parser Command
commandParser = Command <$> udpParser <*> ipParser <*> etherParser
                        <*> (payloadType <|> pure ("uchar", 1))
                        <*> payloadLength <*> output
  where
    output :: Parser FilePath
    output = outputFilePath <|> pure "/dev/stdout"
      where
        outputFilePath = strOption $ mconcat
            [ metavar "PATH", short 'o', long "output"
            , help "Output file to use.", showDefault ]

    payloadLength :: Parser Word16
    payloadLength = option auto $ mconcat
        [ metavar "N", short 'l', long "length"
        , help "Payload length."
        ]

    payloadType :: Parser (String, Word16)
    payloadType = (,) <$> openclType <*> openclSize

    openclType :: Parser String
    openclType = strOption $ mconcat
        [ metavar "TYPE", short 't', long "type"
        , help "Payload type.", value "uchar"
        ]

    openclSize :: Parser Word16
    openclSize = option auto $ mconcat
        [ metavar "SIZE", short 's', long "size"
        , help "Size of payload type in bytes."
        ]

commandlineParser :: ParserInfo Command
commandlineParser = info (commandParser <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Generate OpenCL packets."
    ]

encode :: EncodeBits a => a -> ByteString
encode = toZeroPaddedByteString . encodeBits

tagFailure :: String -> Maybe a -> Either String a
tagFailure msg Nothing = Left msg
tagFailure _ (Just v) = Right v

main :: IO ()
main = do
    Command{..} <- execParser commandlineParser

    let udpConfig = udpSpec $ UDPLength (payloadLength * snd payloadType)
        packet = do
            udpPacket <- tagFailure "UDP" $ mkUDP udpConfig
            ipPacket <- tagFailure "IP" $ mkIP (toIpSpec $ UDP2IP udpPacket)
            tagFailure "Ethernet" $
                encode <$> mkEthernetFrame (toEtherSpec (encode ipPacket))

    case packet of
        Left err -> putStrLn err
        Right p -> outputHeader (fst payloadType) payloadLength outputPath p
