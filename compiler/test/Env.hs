module Env (libDirVar, incDirVar) where
import           System.Environment (lookupEnv)

lookup' :: String -> IO String
lookup' name = do
    env <- lookupEnv name
    case env of
      Nothing -> fail $ "Cannot retrieve env " ++ name
      Just s  -> return s

libDirVar :: IO String
libDirVar = lookup' "UPLC2C_LIB_DIR"

incDirVar :: IO String
incDirVar = lookup' "UPLC2C_INC_DIR"

