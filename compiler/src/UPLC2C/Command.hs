{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Command ( main ) where


import           Codec.Serialise             (deserialiseOrFail)
import           Data.ByteString.Lazy        (ByteString, readFile)
import           Data.Text.IO                (writeFile)
import qualified Options.Applicative         as O
import           Plutus.V1.Ledger.Scripts    (Script (..))

import           UPLC2C.Compile              (compile)
import           UPLC2C.Prelude
import           UPLC2C.Types.CCode          (CCode (..))
import           UPLC2C.Types.InputFilePath  (InputFilePath (..))
import           UPLC2C.Types.OutputFilePath (OutputFilePath (..))

import           Control.Monad               (void)
import           Control.Monad.Except        (runExcept, runExceptT)
import           Data.Bifunctor              (bimap)
import           Debug.Trace                 (trace)
import           PlutusCore                  (DeBruijn)
import           PlutusCore.Error            (Error)
import           PlutusCore.Quote            (runQuote)
import           UntypedPlutusCore           (programMapNames)
import           UntypedPlutusCore           as UPLC (DefaultFun, DefaultUni,
                                                      Program (..),
                                                      deBruijnProgram)
import           UntypedPlutusCore.DeBruijn  (unNameDeBruijn)
import           UntypedPlutusCore.Parser    as UPLC (parseProgram)

data Command = CompileFile Bool InputFilePath OutputFilePath


main :: MonadIO m => m ()
main = runCommand =<< parseCommand


parseCommand :: MonadIO m => m Command
parseCommand = liftIO $ O.execParser commandInfo


inputFilePath :: O.Parser InputFilePath
inputFilePath =
  O.argument (InputFilePath <$> O.str) (O.metavar "INPUT" <> O.help "The input file path")

plc :: O.Parser Bool
plc = O.switch ( O.long "plc" <> O.help "If set file is interpreted as *.plc file instead of CBOR" )

outputFilePath :: O.Parser OutputFilePath
outputFilePath =
  O.argument (OutputFilePath <$> O.str) (O.metavar "OUTPUT" <> O.help "The output file path")


command :: O.Parser Command
command = CompileFile <$> plc <*> inputFilePath <*> outputFilePath


commandInfo :: O.ParserInfo Command
commandInfo =
  O.info (command O.<**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Compile Plutus bytecode to C"
   <> O.header "uplc2c - Untyped Plutus Core to C"
    )


runCommand :: MonadIO m => Command -> m ()
runCommand (CompileFile isPLC inPath outPath) = compileFile isPLC inPath outPath


type UntypedProgram = UPLC.Program DeBruijn DefaultUni DefaultFun
type DefaultError = Error DefaultUni DefaultFun


parsePLC :: ByteString -> Either (DefaultError ()) (UntypedProgram ())
parsePLC bs = do
  x <- (bimap void void . runQuote . runExceptT . UPLC.parseProgram) bs
  toNameless <$> (runExcept . deBruijnProgram) x
  where
    toNameless = programMapNames unNameDeBruijn


compileFile :: MonadIO m => Bool -> InputFilePath -> OutputFilePath -> m ()
compileFile False (InputFilePath inFilePath) (OutputFilePath outFilePath) = do
  inFileBytes <- liftIO $ readFile inFilePath
  case deserialiseOrFail inFileBytes of
    Right (Script (UPLC.Program _ _ term)) -> do
      CCode objectCode <- compile term
      liftIO $ writeFile outFilePath objectCode
    Left _ ->
      liftIO $ putStrLn "input is not a valid UPLC CBOR representation"
compileFile True (InputFilePath inFilePath) (OutputFilePath outFilePath) = do
    inFileBytes <- liftIO $ readFile inFilePath
    case parsePLC inFileBytes of
      Right (UPLC.Program _ _ term) -> do
        CCode objectCode <- trace ("term: " ++ show term) $ compile term
        liftIO $ writeFile outFilePath objectCode
      Left e ->
        liftIO $ putStrLn ("input is not a valid PLC file: " ++ show e)
