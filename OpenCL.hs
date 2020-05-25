{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.ByteString (ByteString)
import Options.Applicative

import EncodeBits
import Ethernet
import IP
import UDP

import OutputHeader (outputHeader)

data Command = Command
    { udpSpec :: UDP
    , toIpSpec :: ByteString -> IP
    , toEtherSpec :: ByteString -> Ethernet
    , outputPath :: FilePath
    }

udpParser :: Parser UDP
udpParser = UDP <$> udpPortParser Nothing "source"
                <*> udpPortParser Nothing "dest"
                <*> pure mempty

ipParser :: Parser (ByteString -> IP)
ipParser = IP <$> ipAddressParser Nothing "source"
              <*> ipAddressParser Nothing "dest"

etherParser :: Parser (ByteString -> Ethernet)
etherParser = Ethernet <$> macAddressParser Nothing "source"
                       <*> macAddressParser Nothing "dest"

commandParser :: Parser Command
commandParser = Command <$> udpParser <*> ipParser <*> etherParser <*> output
  where
    output :: Parser FilePath
    output = outputFilePath <|> pure "/dev/stdout"
      where
        outputFilePath = strOption $ mconcat
            [ metavar "PATH", short 'o', long "output"
            , help "Output file to use.", showDefault ]

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

    let packet = do
            udpData <- tagFailure "UDP" $ encode <$> mkUDP udpSpec
            ipData <- tagFailure "IP" $ encode <$> mkIP (toIpSpec udpData)
            tagFailure "Ethernet" $
                encode <$> mkEthernetFrame (toEtherSpec ipData)

    case packet of
        Left err -> putStrLn err
        Right p -> outputHeader outputPath p
