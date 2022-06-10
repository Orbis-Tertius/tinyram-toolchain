{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Hedgehog                                 as HH
import qualified Hedgehog.Gen                             as Gen
import qualified Hedgehog.Range                           as Range

import           Generator                                ()
import           ToUPLC                                   (NamedUPLCTerm,
                                                           toUPLC)
import qualified UPLC2C.Compile                           as Compile
import           UPLC2C.Types.CCode                       (CCode (CCode))

import           PlutusCore                               (defaultBuiltinsRuntime,
                                                           defaultCekParameters)
import           PlutusCore.Default
import qualified PlutusCore.Error                         as UPLC (Error)
import qualified UntypedPlutusCore                        as UPLC
import           UntypedPlutusCore.Evaluation.Machine.Cek (evaluateCekNoEmit)

import qualified Hedgehog.Gen.QuickCheck                  as HQC
import           Hedgehog.Internal.Property               (Group (Group),
                                                           Property,
                                                           TestLimit (TestLimit),
                                                           footnote, forAll,
                                                           property, withTests,
                                                           (===))
import qualified Test.QuickCheck                          as QC


import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Except               (runExcept)
import qualified Data.ByteString                          as BS
import qualified Data.List                                as L
import           Data.Text                                (unpack)
import           Data.Text.Encoding                       (decodeUtf8)
import           GHC.Exts                                 (toList)
import           System.Directory
import           System.Environment                       (lookupEnv)
import           System.Exit                              (ExitCode (ExitFailure, ExitSuccess))
import           System.IO.Temp                           (emptySystemTempFile,
                                                           writeSystemTempFile)
import           System.Process                           (callCommand)
import           System.Process.ByteString                (readProcessWithExitCode)
import           Text.Printf                              (printf)
import           Text.RawString.QQ                        (r)

newtype Fiziu = Fiziu NamedUPLCTerm
  deriving (Show)

type DeBruijnTerm = UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun
type DefaultError = UPLC.Error UPLC.DefaultUni UPLC.DefaultFun

newtype Show2DeBruijnTerm = Show2DeBruijnTerm (DeBruijnTerm ())

instance Show Show2DeBruijnTerm where
  show (Show2DeBruijnTerm term) = case term of
    UPLC.Var () (UPLC.DeBruijn (UPLC.Index i)) -> "(Idx " ++ show i ++ ")"
    UPLC.LamAbs () _ te                        -> "(Lam " ++ show te ++ ")"
    UPLC.Apply () te te'                       -> "(App " ++ show te ++ " " ++ show te' ++ ")"
    UPLC.Force () te                           -> "(For " ++ show te ++ ")"
    UPLC.Delay () te                           -> "(Del " ++ show te ++ ")"
    UPLC.Constant () so                        -> showConstant so
    UPLC.Builtin () df                         -> show df
    UPLC.Error ()                              -> "error"

showBytestring :: BS.ByteString -> String
showBytestring bs = let l1 = fromIntegral <$> toList bs :: [Integer]
                        l2 = L.concat . L.intersperse "," $ show <$> l1
                     in "{" ++ l2 ++ "}"

showConstant :: Some (ValueOf UPLC.DefaultUni) -> String
showConstant constant = case constant of
    Some (ValueOf DefaultUniInteger v)         -> show v
    Some (ValueOf DefaultUniBool v)            -> show v
    Some (ValueOf DefaultUniUnit v)            -> show v
    Some (ValueOf DefaultUniByteString bs)     -> showBytestring bs
    Some (ValueOf DefaultUniString s)          -> show s
    Some (ValueOf (DefaultUniPair xUni yUni) (x, y)) ->
      "(" ++ showConstant (Some (ValueOf xUni x)) ++
      "," ++ showConstant (Some (ValueOf yUni y)) ++
      ")"
    Some (ValueOf (DefaultUniList eUni) list)     ->
      let l = showConstant . Some . ValueOf eUni <$> list in
          "[" ++ L.intercalate "," l ++ "]"
    _ -> undefined

instance QC.Arbitrary Fiziu where
  arbitrary = do
    dsl <- QC.arbitrary
    let uplc = toUPLC dsl
    return (Fiziu uplc)

libDirVar :: String
libDirVar = "UPLC2C_LIB_DIR"

incDirVar :: String
incDirVar = "UPLC2C_INC_DIR"

data Abort = Abort

compile :: String -> String -> DeBruijnTerm () -> IO (Either Abort String)
compile incDir libDir term = do
  CCode compiled <- Compile.compile term
  srcPath <- writeSystemTempFile "testcase.c" (unpack compiled)
  binPath <- emptySystemTempFile "testcase.bin"
  callCommand (printf command incDir binPath srcPath libDir)
  (exitCode, stdout, _) <- readProcessWithExitCode binPath [] mempty
  removeFile srcPath
  removeFile binPath
  case exitCode of
    ExitSuccess      -> return $ Right $ Data.Text.unpack $ decodeUtf8 stdout
    ExitFailure (-6) -> return $ Left Abort
    ExitFailure _    -> fail "Unexpected process exit code"
  where
    command :: String
    command =
      [r|
/nix/store/rfdx35kbgh9xx25x67jcl9zhshqhy794-gcc-wrapper-10.3.0/bin/c++ \
  -I %s \
  -o %s \
  %s \
  %s/libuplc2c_rts.a \
      |]

namedToDeBruijn :: NamedUPLCTerm -> Either (DefaultError ()) (DeBruijnTerm ())
namedToDeBruijn t =
  UPLC.termMapNames UPLC.unNameDeBruijn <$> (runExcept . UPLC.deBruijnTerm) t

compileRun :: NamedUPLCTerm -> IO (Either Abort String)
compileRun namedTerm = do
    env <- (,) <$> lookupEnv libDirVar <*> lookupEnv incDirVar
    (libDir, incDir) <- case env of
      (Just libDir, Just incDir) -> return (libDir, incDir)
      _                          -> fail "vars not set"
    case namedToDeBruijn namedTerm of
      Left err   -> fail (show err)
      Right term -> compile incDir libDir term

f :: Either e1 (Either e2 a) -> Either (Either e1 e2) a
f (Left x)          = Left (Left x)
f (Right (Left x))  = Left (Right x)
f (Right (Right x)) = Right x

prop_plutus :: Property
prop_plutus =
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
main = HH.checkSequential $ Group "Test.Example" [
      ("prop_plutus", withTests (TestLimit 300) prop_plutus)
    ]
