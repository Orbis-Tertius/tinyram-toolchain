{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Hedgehog                   as HH

import           Hedgehog.Internal.Property (Group (Group),
                                             TestLimit (TestLimit), withTests)

import           SCApplicationTest          (prop_datum_ingestion,
                                             prop_redeemer_ingestion,
                                             prop_script_context_ingestion)
import           SerializationTest          (prop_serdes_fault_injection,
                                             prop_serdes_roundtrip)
import           UPLC2CTest                 (prop_uplc2c)

main :: IO Bool
main = HH.checkSequential $ Group "Test.Example" [
         ("prop_script_context_ingestion", withTests (TestLimit 100) prop_script_context_ingestion)
       , ("prop_datum_ingestion", withTests (TestLimit 100) prop_datum_ingestion)
       , ("prop_redeemer_ingestion", withTests (TestLimit 100) prop_redeemer_ingestion)
       , ("prop_serdes_fault_injection", withTests (TestLimit 1000) prop_serdes_fault_injection)
       , ("prop_serdes_roundtrip", withTests (TestLimit 1000) prop_serdes_roundtrip)
       , ("prop_uplc2c", withTests (TestLimit 300) prop_uplc2c)
    ]
