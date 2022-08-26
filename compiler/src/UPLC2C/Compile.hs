{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module UPLC2C.Compile ( compile, Mode(..) ) where


import qualified Data.Map                         as Map
import           Data.Text                        (pack)
import           Text.Printf                      (printf)

import           UPLC2C.CompileFunctionDefinition (compileFunctionDefinition)
import           UPLC2C.Prelude
import           UPLC2C.TermToCProgram            (termToCProgram)
import           UPLC2C.Types.CCode               (CCode (..))
import           UPLC2C.Types.CName               (CName (..))
import           UPLC2C.Types.CProgram            (CProgram (..))
import           UPLC2C.Types.CProgramBuilder     (CProgramBuilder (getProgram))
import qualified UPLC2C.Types.CProgramBuilderT    as CProgramBuilderT
import           UPLC2C.Types.UPLCTerm            (UPLCTerm)

data Mode = Standalone | Validator

compile :: MonadIO m => Mode -> UPLCTerm -> m CCode
compile mode term = do
  (program, entryPointName) <- CProgramBuilderT.run $ do
    entryPointName <- termToCProgram term
    program <- getProgram
    return (program, entryPointName)
  let code = CCode "#include \"rts.h\"\n\n" <> programCode program <> case mode of
        Standalone ->
          CCode (pack (printf "\n\n\n\nint main() { const struct NFData * data = %s(0);                    print(data); return 0; }\n" (unCName entryPointName)))
        Validator ->
          CCode (pack (printf "\n\n\n\nint main() { const struct NFData * data = apply_script_args (& %s); print(data); return 0; }\n" (unCName entryPointName)))
  return code


programCode :: CProgram -> CCode
programCode program = programFunctionSignatures program <> CCode "\n\n\n\n" <> programFunctionDefinitions program


programFunctionSignatures :: CProgram -> CCode
programFunctionSignatures (CProgram program) =
  mconcat $ functionSignature <$> Map.keys program


functionSignature :: CName -> CCode
functionSignature (CName name) =
  CCode . pack $ printf "static const struct NFData *%s(const struct LexicalScope *scope);\n" name


programFunctionDefinitions :: CProgram -> CCode
programFunctionDefinitions (CProgram program) =
  mconcat . Map.elems $ Map.mapWithKey compileFunctionDefinition program
