{-# LANGUAGE NoImplicitPrelude #-}


module UPLC2C.Types.CFunctionDefinition ( CFunctionDefinition (..) ) where


import           UPLC2C.Prelude
import           UPLC2C.Types.CName         (CName)
import           UPLC2C.Types.DeBruijnIndex (DeBruijnIndex)

import qualified Data.ByteString            as BS

data CFunctionDefinition =
    VariableReference DeBruijnIndex
  | CreateClosureOver CName
  | Apply CName CName
  | Delay CName
  | Force CName
  | ConstantInteger Integer
  | ConstantBool Bool
  | ConstantUnit
  | ConstantByteString BS.ByteString
  | ConstantString Text
  | ConstantPair CName CName
  | ConstantList [CName]
  | ConstantDataConstr CName CName
  | ConstantDataMap CName
  | ConstantDataList CName
  | ConstantDataInteger CName
  | ConstantDataByteString CName
  -- TODO constants
  deriving (Eq, Ord, Show)
