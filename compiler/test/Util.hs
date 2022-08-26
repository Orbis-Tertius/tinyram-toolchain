module Util (toBS, withTempFile) where

import           Data.Bits        (Bits (shiftR, (.&.)))
import qualified Data.ByteString  as BS
import           Data.Word        (Word32)
import           GHC.Exts         (fromList)
import           GHC.IO.Handle    (hFlush)
import           System.Directory (removeFile)
import           System.IO.Temp   (openBinaryTempFile)

toBS :: [Word32] -> BS.ByteString
toBS l = fromList $ toBytes =<< l
  where
    toBytes w = fromIntegral . (.&. 0xff) <$> [w, w `shiftR` 8,  w `shiftR` 16,  w `shiftR` 24]

withTempFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempFile bs handler = do
  (path, handle) <- openBinaryTempFile "/tmp" "serializedBS.bin"
  BS.hPut handle bs
  hFlush handle
  r <- handler path
  removeFile path
  return r
