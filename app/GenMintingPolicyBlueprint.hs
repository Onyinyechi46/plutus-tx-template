{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AuctionMintingPolicy
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

--------------------------------------------------------------------------------
-- | Contract Blueprint Definition
--------------------------------------------------------------------------------

-- | Full contract blueprint for the minting policy
myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-minting-policy"
    , contractPreamble = contractPreamble
    , contractValidators = Set.singleton mintingValidator
    , contractDefinitions = deriveDefinitions @[AuctionMintingParams, ()]
    }

-- | Metadata describing the contract
contractPreamble :: Preamble
contractPreamble =
  MkPreamble
    { preambleTitle = "Auction Minting Policy"
    , preambleDescription = Just "A simple minting policy"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

--------------------------------------------------------------------------------
-- | Minting Validator Blueprint
--------------------------------------------------------------------------------

-- | The validator blueprint describing the minting policy
mintingValidator :: ValidatorBlueprint referencedTypes
mintingValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Minting Validator"
    , validatorDescription = Just "A simple minting validator"
    , validatorParameters = [mintingValidatorParams]
    , validatorRedeemer = unitRedeemer
    , validatorDatum = Nothing
    , validatorCompiled = compiledPolicy
    }

-- | Description of the validator parameters
mintingValidatorParams :: ParameterBlueprint
mintingValidatorParams =
  MkParameterBlueprint
    { parameterTitle = Just "Minting Validator Parameters"
    , parameterDescription = Just "Compile-time validator parameters"
    , parameterPurpose = Set.singleton Mint
    , parameterSchema = definitionRef @AuctionMintingParams
    }

-- | The validator uses `()` as the redeemer
unitRedeemer :: ArgumentBlueprint
unitRedeemer =
  MkArgumentBlueprint
    { argumentTitle = Just "Redeemer for the minting policy"
    , argumentDescription = Just "The minting policy does not use a redeemer, hence ()"
    , argumentPurpose = Set.singleton Mint
    , argumentSchema = definitionRef @()
    }

-- | The compiled script to be used for the validator
compiledPolicy :: Maybe CompiledValidator
compiledPolicy = 
  let
    -- ⚠️ Replace the following `error` with actual parameter or CLI input
    dummySellerPkh = error "TODO: Replace with seller's public key hash"
    script = auctionMintingPolicyScript dummySellerPkh
    code = Short.fromShort (serialiseCompiledCode script)
  in
    Just (compiledValidator PlutusV2 code)

--------------------------------------------------------------------------------
-- | CLI Entrypoint
--------------------------------------------------------------------------------

-- | Writes the blueprint JSON to a file specified in the CLI
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main = getArgs >>= \case
  [outputFile] -> writeBlueprintToFile outputFile
  args -> do
    putStrLn "Usage: auction-minting-blueprint <output-path>"
    fail $ "Expected 1 argument, but got " <> show (length args)
