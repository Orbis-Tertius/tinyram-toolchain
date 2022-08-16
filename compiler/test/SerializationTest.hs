{-# LANGUAGE OverloadedStrings #-}

module SerializationTest (prop_serdes) where

import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Bits                            (Bits (shiftR), (.&.))
import qualified Data.ByteString                      as BS
import           Data.Word                            (Word32)
import           GHC.Exts                             (fromList)
import           GHC.IO.Handle                        (hFlush)
import           Hedgehog                             (Group (Group), Property,
                                                       annotate,
                                                       checkSequential,
                                                       footnote, forAll,
                                                       property, withTests,
                                                       (===))
import qualified Hedgehog                             as HH
import qualified Hedgehog.Gen.QuickCheck              as HQC
import           Hedgehog.Internal.Property           (TestLimit (TestLimit))
import           PlutusCore.Data
import           ShowInstances
import           System.Directory                     (removeFile)
import           System.Environment                   (lookupEnv)
import           System.Exit
import           System.IO                            (openBinaryTempFile)
import           System.Process                       (readProcessWithExitCode)
import qualified Test.QuickCheck                      as QC
import           Test.QuickCheck.Instances.ByteString
import           UPLC2C.Serialization

instance QC.Arbitrary Data where
  arbitrary = arbitrary' 0
    where
      arbitrary' :: Integer -> QC.Gen Data
      arbitrary' n = if n > 8
        then QC.oneof [bGen, iGen]
        else QC.oneof [bGen, iGen, mapGen (n + 1), listGen (n + 1), constrGen (n + 1)]

      bGen = B <$> QC.resize 32 QC.arbitrary
      iGen = I <$> QC.arbitrary

      listOf' n = QC.listOf $ arbitrary' n
      mapGen n = Map <$> QC.resize 5 (zip <$> listOf' n <*> listOf' n)
      listGen n = List <$> QC.resize 5 (listOf' n)
      constrGen n = uncurry Constr <$> ((,) <$> QC.arbitrary <*> QC.resize 5 (listOf' n))

libDirVar :: String
libDirVar = "UPLC2C_LIB_DIR"

deserializeBinPath :: IO String
deserializeBinPath = do
    env <- lookupEnv libDirVar
    case env of
      Nothing -> fail ""
      Just s  -> return $ s ++ "/bin/deserialize"

toBS :: [Word32] -> BS.ByteString
toBS l = fromList $ toBytes =<< l
  where
    toBytes w = fromIntegral . (.&. 0xff) <$> [w, w `shiftR` 8,  w `shiftR` 16,  w `shiftR` 24]

executeDeserializeBin :: [Word32] -> IO (Either String String)
executeDeserializeBin list = do
  bin <- deserializeBinPath
  (filePath, handle) <- openBinaryTempFile "." "serialized.bin"
  BS.hPut handle (toBS list)
  hFlush handle
  (exitCode, stdout, err) <- readProcessWithExitCode bin [filePath] mempty
  removeFile filePath
  case exitCode of
    ExitSuccess   -> return $ Right stdout
    ExitFailure n -> return $ Left $ "deserialize exited with an error: " ++ show n

prop_serdes :: Property
prop_serdes =
  property $ do
    data' <- forAll HQC.arbitrary
    let serialized = serialize data'
        deserialized = deserialize serialized
    deserialized' <- liftIO $ executeDeserializeBin serialized
    case (deserialized', deserialized) of
      (Left s, _) -> do
        footnote s
        HH.failure
      (_, Nothing)           -> HH.failure
      (_, Just (_, x:_))     -> HH.failure
      (Right deserialized'' , Just (data'', [])) -> do
        data' === data''
        show (Show2Data data') === deserialized''
