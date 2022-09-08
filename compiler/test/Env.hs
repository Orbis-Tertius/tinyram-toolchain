module Env (x86LibDirVar, incDirVar, tinyRAMLibDirVar) where
import           System.Environment (lookupEnv)

lookup' :: String -> IO String
lookup' name = do
    env <- lookupEnv name
    case env of
      Nothing -> fail $ "Cannot retrieve env " ++ name
      Just s  -> return s

x86LibDirVar :: IO String
x86LibDirVar = lookup' "UPLC2C_X86_LIB_DIR"

tinyRAMLibDirVar :: IO String
tinyRAMLibDirVar = lookup' "UPLC2C_TINYRAM_LIB_DIR"

incDirVar :: IO String
incDirVar = lookup' "UPLC2C_INC_DIR"

