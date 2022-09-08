{-# LANGUAGE OverloadedStrings #-}

module SCApplicationTest (
  prop_x86_script_context_ingestion,
  prop_x86_datum_ingestion,
  prop_x86_redeemer_ingestion,
  prop_tinyram_script_context_ingestion,
  prop_tinyram_datum_ingestion,
  prop_tinyram_redeemer_ingestion) where

import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString           as BS
import           DataGenerator
import           Debug.Trace               (trace)
import           Env                       (tinyRAMLibDirVar, x86LibDirVar)
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
                                            readCreateProcessWithExitCode,
                                            readProcessWithExitCode)
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

x86TestBinary :: Int -> IO String
x86TestBinary number = do
  env <- x86LibDirVar
  return $ env ++ "/uplc-testcases/test_validator_" ++ show number

tinyRAMTestBinary :: Int -> IO String
tinyRAMTestBinary number = do
  env <- tinyRAMLibDirVar
  return $ env ++ "/uplc-testcases/test_validator_" ++ show number ++ ".bin"

x86Execute :: String -> String -> IO (ExitCode, String, String)
x86Execute binPath publicTapePath = readCreateProcessWithExitCode (proc' { env = nEnv }) ""
 where
     proc' = proc binPath []
     t = ("PUBLIC_TAPE_FILEPATH", publicTapePath)
     nEnv = Just $ case env proc' of
       Nothing -> [t]
       Just x0 -> t:x0

tinyRAMExecute :: String -> String -> String -> IO (ExitCode, String, String)
tinyRAMExecute binPath publicTapePath auxiliaryTapePath = readProcessWithExitCode
  "tinyram"
  ["-w", "32", "-r", "16", "--max-steps", "20000000", binPath, publicTapePath, auxiliaryTapePath]
  ""

type Executor = String -> String -> IO (ExitCode, String)

x86Execute' :: Int -> Executor
x86Execute' number publicTapePath _ = do
    x86BinPath <- x86TestBinary number
    (ec, stdout, _) <- x86Execute x86BinPath publicTapePath
    return (ec, stdout)

tinyRAMExecute' :: Int -> Executor
tinyRAMExecute' number publicTapePath privateTapePath = do
    tinyRAMBinPath <- tinyRAMTestBinary number
    (ec, _, stderr) <- tinyRAMExecute tinyRAMBinPath publicTapePath privateTapePath
    return (ec, stderr)

prop_arg_ingestion :: Executor -> (PublicTape -> Data) -> Property
prop_arg_ingestion executor lens =
  property $ do
    tape <- forAll (HQC.quickcheck genPublicTape)
    let serialized = serializePublicTape tape
    r <- lift $ withTempFile serialized $ \publicTapePath -> do
                withTempFile BS.empty $ \privateTapePath -> do
                  executor publicTapePath privateTapePath
    case r of
        (ExitSuccess, s) -> show (Show2Data (lens tape)) === s
        (_, _)           -> HH.failure

prop_x86_script_context_ingestion :: Property
prop_x86_script_context_ingestion = prop_arg_ingestion (x86Execute' 1) sc

prop_x86_datum_ingestion :: Property
prop_x86_datum_ingestion = prop_arg_ingestion (x86Execute' 2) datum

prop_x86_redeemer_ingestion :: Property
prop_x86_redeemer_ingestion = prop_arg_ingestion (x86Execute' 3) redeemer

prop_tinyram_script_context_ingestion :: Property
prop_tinyram_script_context_ingestion = prop_arg_ingestion (tinyRAMExecute' 1) sc

prop_tinyram_datum_ingestion :: Property
prop_tinyram_datum_ingestion = prop_arg_ingestion (tinyRAMExecute' 2) datum

prop_tinyram_redeemer_ingestion :: Property
prop_tinyram_redeemer_ingestion = prop_arg_ingestion (tinyRAMExecute' 3) redeemer
