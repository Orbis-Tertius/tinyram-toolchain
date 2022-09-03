{-# LANGUAGE OverloadedStrings #-}

module SCApplicationTest (prop_script_context_ingestion, prop_datum_ingestion, prop_redeemer_ingestion) where

import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString           as BS
import           DataGenerator
import           Env                       (libDirVar)
import           GHC.IO.Handle             (Handle, hFlush)
import           Hedgehog                  (Property, forAll, property, (===))
import qualified Hedgehog                  as HH
import qualified Hedgehog.Gen.QuickCheck   as HQC
import           PlutusCore.Data           (Data)
import           ShowInstances             (Show2Data (Show2Data))
import           System.Directory          (removeFile)
import           System.Exit
import           System.IO.Temp            (openBinaryTempFile)
import           System.Process            (CreateProcess (env), proc,
                                            readCreateProcessWithExitCode)
import qualified Test.QuickCheck           as QC
import           UPLC2C.Serialization      (serialize)
import           Util                      (toBS, withTempFile)

data PublicTape = PublicTape {
    sc       :: Data,
    datum    :: Data,
    redeemer :: Data
  } deriving (Show)

genPublicTape :: QC.Gen PublicTape
genPublicTape = PublicTape <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

serializePublicTape :: PublicTape -> BS.ByteString
serializePublicTape (PublicTape sc datum redeemer) =
     len' sc' <> sc'
  <> len' datum' <> datum'
  <> len' redeemer' <> redeemer'
  where
    serialize' = toBS . serialize
    len' bs = toBS [fromIntegral $ BS.length bs `div` 4]

    sc' = serialize' sc
    datum' = serialize' datum
    redeemer' = serialize' redeemer

testBinary :: Int -> IO String
testBinary number = do
  env <- libDirVar
  return $ env ++ "/uplc-testcases/test_validator_" ++ show number

executeWithEnvVar :: String -> String -> IO (ExitCode, String, String)
executeWithEnvVar binPath publicTapePath = readCreateProcessWithExitCode (proc' { env = nEnv }) ""
 where
     proc' = proc binPath []
     t = ("PUBLIC_TAPE_FILEPATH", publicTapePath)
     nEnv = Just $ case env proc' of
       Nothing -> [t]
       Just x0 -> t:x0

prop_arg_ingestion :: Int -> (PublicTape -> Data) -> Property
prop_arg_ingestion number lens =
  property $ do
    tape <- forAll (HQC.quickcheck genPublicTape)
    let serialized = serializePublicTape tape
    r <- lift $ withTempFile serialized $ \publicTapePath -> do
      binPath <- testBinary number
      executeWithEnvVar binPath publicTapePath
    case r of
        (ExitSuccess, stdout, _) -> show (Show2Data (lens tape)) === stdout
        (ExitFailure _, _, _)    -> HH.failure

prop_script_context_ingestion :: Property
prop_script_context_ingestion = prop_arg_ingestion 1 sc

prop_datum_ingestion :: Property
prop_datum_ingestion = prop_arg_ingestion 2 datum

prop_redeemer_ingestion :: Property
prop_redeemer_ingestion = prop_arg_ingestion 3 redeemer
