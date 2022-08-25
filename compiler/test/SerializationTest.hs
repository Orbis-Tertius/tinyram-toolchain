{-# LANGUAGE OverloadedStrings #-}

module SerializationTest (prop_serdes_roundtrip, prop_serdes_fault_injection) where

import           Control.Lens                         (element, (&), (.~))
import           Control.Monad                        (join)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Bits                            (Bits (shiftR), (.&.))
import qualified Data.ByteString                      as BS
import           Data.Functor.Identity
import           Data.Word                            (Word32)
import           GHC.Exts                             (fromList)
import           GHC.IO.Handle                        (hFlush)
import           Hedgehog                             (GenT, Group (Group),
                                                       Property, annotate,
                                                       checkSequential,
                                                       footnote, forAll,
                                                       property, withTests,
                                                       (===))
import qualified Hedgehog                             as HH
import           Hedgehog.Gen                         (choice, frequency,
                                                       integral, word32)
import qualified Hedgehog.Gen.QuickCheck              as HQC
import           Hedgehog.Internal.Property           (TestLimit (TestLimit))
import qualified Hedgehog.Range                       as Range
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

      bGen = B <$> QC.resize 16 QC.arbitrary
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

newtype DeserializeError = DeserializeError Int
  deriving (Eq)

instance Show DeserializeError where
  show (DeserializeError c) = "Deserialization failed with error code: " ++ show c

executeDeserializeBin :: [Word32] -> IO (Either DeserializeError (String, [Word32]))
executeDeserializeBin list = do
  bin <- deserializeBinPath
  (filePath, handle) <- openBinaryTempFile "." "serialized.bin"
  BS.hPut handle (toBS list)
  hFlush handle
  (exitCode, stdout, err) <- readProcessWithExitCode bin [filePath] mempty
  removeFile filePath
  case exitCode of
    ExitSuccess -> do
      case lines stdout of
        [deserialized, residue] -> return $ Right (deserialized, read residue)
        _                       -> fail "Unexpected binary output"
    ExitFailure n -> return $ Left $ DeserializeError n

prop_serdes_roundtrip :: Property
prop_serdes_roundtrip =
  property $ do
    data' <- forAll HQC.arbitrary
    let serialized = serialize data'
        deserialized = deserialize serialized
    deserialized' <- liftIO $ executeDeserializeBin serialized
    case (deserialized', deserialized) of
      (Left s, _) -> do
        footnote (show s)
        HH.failure
      (Right (deserialized'', []) , Just (data'', [])) -> do
        data' === data''
        show (Show2Data data') === deserialized''
      _ -> HH.failure

injectFault :: [Word32] -> HH.Gen [Word32]
injectFault l = do
  pos <- integral (Range.constant 0 (length l - 1))
  sub <- word32 (Range.constant 0 32)
  let newL = l & element pos .~ sub
  return newL

truncate' :: [Word32] -> HH.Gen [Word32]
truncate' l = do
  pos <- integral (Range.constant 0 (length l - 1))
  let newL = take pos l
  return newL

alter :: [Word32] -> HH.Gen [Word32]
alter l = frequency [(3, injectFault l), (1, truncate' l)]

prop_serdes_fault_injection :: Property
prop_serdes_fault_injection =
  property $ do
    data' <- forAll HQC.arbitrary
    let serialized = serialize data'
    alteredSerialized <- forAll (alter serialized)
    let deserialized = deserialize alteredSerialized
    deserialized' <- liftIO $ executeDeserializeBin alteredSerialized
    case (deserialized', deserialized) of
      (Right (deserialized'', residue1) , Just (data'', residue2)) -> do
        show (Show2Data data'') === deserialized''
        residue1 === residue2
      (Left n, Nothing) | n == sigabt -> HH.success
      (Left n, _) -> do
        HH.footnote (show n)
        HH.failure
      _ -> HH.failure
    where
      sigabt = DeserializeError (-6)

