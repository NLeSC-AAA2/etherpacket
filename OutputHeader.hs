{-# LANGUAGE QuasiQuotes #-}
module OutputHeader (outputHeader) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isAlphaNum, toUpper)
import Data.String.Interpolate (i)
import Data.Word (Word16)
import Numeric (showHex)
import System.FilePath (takeFileName)
import System.IO (IOMode(WriteMode), hPutStr, hPutStrLn, withFile)

outputHeader :: String -> Word16 -> FilePath -> ByteString -> IO ()
outputHeader openclType len path header = withFile path WriteMode $ \hnd -> do
    hPutStrLn hnd $ "#ifndef " ++ cppMacro
    hPutStrLn hnd $ "#define " ++ cppMacro
    hPutStrLn hnd $ prefixCode openclType len
    forM_ (BS.unpack header) $ \byte -> do
        hPutStr hnd ("0x" <> showHex byte ", ")
    hPutStrLn hnd "\n};"
    hPutStrLn hnd "#endif"
  where
    cppMacro = map replaceChar $ takeFileName path

    replaceChar c
        | isAscii c && isAlphaNum c = toUpper c
        | otherwise = '_'

prefixCode :: String -> Word16 -> String
prefixCode openclType size = [i|
struct packet {
  struct ethernet_header {
    uchar destination_mac[6], source_mac[6];
    ushort ether_type;
  } ethernet_header;

  struct ipv4_header {
    uchar version_ihl, dscp_ecn;
    ushort length;
    ushort identification, flags_fragment_offset;
    uchar  ttl, protocol;
    ushort checksum;
    uint   source_ip_address, destination_ip_address;
  } __attribute__((packed)) ipv4_header;

  struct udp_header {
    ushort source_port, destination_port;
    ushort length, checksum;
  } udp_header;

  #{openclType} payload[#{size}];
} __attribute__((packed, aligned(sizeof(int8))));

__constant uchar packet_header[42] __attribute__((aligned(32))) =
{|]
