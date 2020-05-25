{-# LANGUAGE QuasiQuotes #-}
module OutputHeader (outputHeader) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isAlphaNum, toUpper)
import Data.String.Interpolate (i)
import Numeric (showHex)
import System.FilePath (takeFileName)
import System.IO (IOMode(WriteMode), hPutStr, hPutStrLn, withFile)

outputHeader :: FilePath -> ByteString -> IO ()
outputHeader path packetHeader = withFile path WriteMode $ \hnd -> do
    hPutStrLn hnd $ "#ifndef " ++ cppMacro
    hPutStrLn hnd $ "#define " ++ cppMacro
    hPutStrLn hnd prefixCode
    forM_ (BS.unpack packetHeader) $ \byte -> do
        hPutStr hnd ("0x" <> showHex byte ", ")
    hPutStrLn hnd suffixCode
    hPutStrLn hnd "#endif"
  where
    cppMacro = map replaceChar $ takeFileName path

    replaceChar c
        | isAscii c && isAlphaNum c = toUpper c
        | otherwise = '_'

prefixCode :: String
prefixCode = [i|
struct packet_header {
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

  uchar payload[];
} __attribute__((packed, aligned(sizeof(int8))));

__constant uchar raw_packet_data[42] __attribute__((aligned(32))) =
{|]

suffixCode :: String
suffixCode = [i|
};

ushort htons(ushort n)
{
#if defined __ENDIAN_LITTLE__
  return as_ushort(as_uchar2(n).yx);
#else
  return n;
#endif
}

ushort ntohs(ushort n)
{
  return htons(n);
}

void checksum(struct ipv4_header *header)
{
    header->checksum = 0;

    uint32_t intermediate = 0;
    ushort *header_bytes = (ushort*) header;
    for (int i = 0; i < (sizeof (struct ipv4_header) / sizeof (ushort)); i++) {
        intermediate += ntohs(header_bytes[i]);
    }

    const uint32_t maxVal = 0xFFFF;
    while (intermediate > maxVal) {
        intermediate = (intermediate & 0xFFFF) + (intermediate >> 16);
    }

    header->checksum = htons(~intermediate);
}

void set_header_payload_size(struct packet_header *header, ushort size)
{
    ushort udp_size = size + sizeof (struct udp_header);
    header->udp_header.length = htons(udp_size);
    header->ipv4_header.length = htons(udp_size + sizeof (struct ipv4_header));
    checksum(header->ipv4_header);
}|]
