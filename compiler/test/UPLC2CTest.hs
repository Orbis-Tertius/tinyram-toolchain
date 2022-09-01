{-# LANGUAGE QuasiQuotes #-}

module UPLC2CTest (prop_uplc2c) where

import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Except               (runExcept)
import           Data.Text
import qualified Data.Text
import           Data.Text.Encoding                       (decodeUtf8)
import           Env                                      (incDirVar,
                                                           x86LibDirVar)
import           ExprGenerator
import           Hedgehog
import qualified Hedgehog                                 as HH
import qualified Hedgehog.Gen.QuickCheck                  as HQC
import           PlutusCore                               (defaultCekParameters)
import qualified PlutusCore.Default                       as UPLC
import qualified PlutusCore.Error                         as UPLC
import           ShowInstances                            (Show2DeBruijnTerm (Show2DeBruijnTerm))
import           System.Directory                         (removeFile)
import           System.Environment                       (lookupEnv)
import           System.Exit
import           System.IO.Temp
import           System.Process
import qualified Test.QuickCheck                          as QC
import           Text.Printf                              (printf)
import           Text.RawString.QQ                        (r)
import           ToUPLC                                   (NamedUPLCTerm,
                                                           toUPLC)
import           UPLC2C.Compile                           (Mode (Standalone))
import qualified UPLC2C.Compile                           as Compile
import           UPLC2C.Types.CCode
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek (evaluateCekNoEmit)

newtype Fiziu = Fiziu NamedUPLCTerm
  deriving (Show)

type DeBruijnTerm = UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun
type DefaultError = UPLC.Error UPLC.DefaultUni UPLC.DefaultFun

instance QC.Arbitrary Fiziu where
  arbitrary = do
    dsl <- QC.arbitrary
    let uplc = toUPLC dsl
    return (Fiziu uplc)

data Abort = Abort

compile :: String -> String -> DeBruijnTerm () -> IO (Either Abort String)
compile incDir libDir term = do
  CCode compiled <- Compile.compile Standalone term
  srcPath <- writeSystemTempFile "testcase.c" (unpack compiled)
  binPath <- emptySystemTempFile "testcase.bin"
  callCommand (printf command incDir incDir binPath srcPath libDir libDir)
  (exitCode, stdout, _) <- readProcessWithExitCode binPath [] mempty
  removeFile srcPath
  removeFile binPath
  case exitCode of
    ExitSuccess      -> return $ Right stdout
    ExitFailure (-6) -> return $ Left Abort
    ExitFailure _    -> fail "Unexpected process exit code"
  where
    command :: String
    command =
      [r|
c++ \
  -I %s/rts/ \
  -I %s/gmp/ \
  -o %s \
  %s \
  %s/rts/librts.a \
  %s/gmp/libgmp.a
      |]

namedToDeBruijn :: NamedUPLCTerm -> Either (DefaultError ()) (DeBruijnTerm ())
namedToDeBruijn t =
  UPLC.termMapNames UPLC.unNameDeBruijn <$> (runExcept . UPLC.deBruijnTerm) t

compileRun :: NamedUPLCTerm -> IO (Either Abort String)
compileRun namedTerm = do
    (libDir, incDir) <- (,) <$> x86LibDirVar <*> incDirVar
    case namedToDeBruijn namedTerm of
      Left err   -> fail (show err)
      Right term -> compile incDir libDir term

f :: Either e1 (Either e2 a) -> Either (Either e1 e2) a
f (Left x)          = Left (Left x)
f (Right (Left x))  = Left (Right x)
f (Right (Right x)) = Right x

prop_uplc2c :: Property
prop_uplc2c =
  property $ do
    (Fiziu c) <- forAll HQC.arbitrary
    let pl' = evaluateCekNoEmit defaultCekParameters c
    let pl = Show2DeBruijnTerm <$> f (namedToDeBruijn <$> pl')
    stdOut <- lift $ compileRun c
    footnote $ "Original: " ++ show c
    case (stdOut, show <$> pl) of
      (Right uplc2c, Right plutus) -> uplc2c === plutus
      (Left _, Left (Left _))      -> HH.success
      (Left _, Left (Right _))     -> HH.failure
      _                            -> HH.failure
