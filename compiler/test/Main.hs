{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Hedgehog                   as HH

import           Hedgehog.Internal.Property (Group (Group),
                                             TestLimit (TestLimit), withTests)

import           SCApplicationTest          (prop_tinyram_datum_ingestion,
                                             prop_tinyram_redeemer_ingestion,
                                             prop_tinyram_script_context_ingestion,
                                             prop_x86_datum_ingestion,
                                             prop_x86_redeemer_ingestion,
                                             prop_x86_script_context_ingestion)
import           SerializationTest          (prop_serdes_fault_injection,
                                             prop_serdes_roundtrip)
import           UPLC2CTest                 (prop_uplc2c)

main :: IO Bool
main = HH.checkSequential $ Group "Test.Example" [
         ("prop_x86_script_context_ingestion", withTests (TestLimit 10) prop_x86_script_context_ingestion)
       , ("prop_x86_datum_ingestion", withTests (TestLimit 10) prop_x86_datum_ingestion)
       , ("prop_x86_redeemer_ingestion", withTests (TestLimit 10) prop_x86_redeemer_ingestion)
       , ("prop_tinyram_script_context_ingestion", withTests (TestLimit 25) prop_tinyram_script_context_ingestion)
       , ("prop_tinyram_datum_ingestion", withTests (TestLimit 25) prop_tinyram_datum_ingestion)
       , ("prop_tinyram_redeemer_ingestion", withTests (TestLimit 25) prop_tinyram_redeemer_ingestion)
       , ("prop_serdes_fault_injection", withTests (TestLimit 1000) prop_serdes_fault_injection)
       , ("prop_serdes_roundtrip", withTests (TestLimit 1000) prop_serdes_roundtrip)
       , ("prop_uplc2c", withTests (TestLimit 300) prop_uplc2c)
    ]
