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

import AuctionMintingPolicy (AuctionMintingParams, auctionMintingPolicyScript)
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

-- | Top-level contract blueprint for the auction minting policy
myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "auction-minting-policy"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[AuctionMintingParams, ()]
    }

-- | Metadata describing the contract
myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Auction Minting Policy"
    , preambleDescription = Just "A simple minting policy"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

-- | Build the compiled validator
compiledAuctionValidator :: Maybe CompiledValidator
compiledAuctionValidator = 
  let 
    -- TODO: Replace this with a real public key hash or pass it as a parameter
    dummySellerPKH = error "Replace with seller public key hash"
    script = auctionMintingPolicyScript dummySellerPKH
    code = Short.fromShort (serialiseCompiledCode script)
  in 
    Just (compiledValidator PlutusV2 code)

-- | Validator blueprint for the minting policy
myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Auction Minting Validator"
    , validatorDescription = Just "A simple minting validator"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Minting Validator Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @AuctionMintingParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer for the minting policy"
          , argumentDescription = Just "The minting policy does not use a redeemer, hence ()"
          , argumentPurpose = Set.singleton Mint
          , argumentSchema = definitionRef @()
          }
    , validatorDatum = Nothing
    , validatorCompiled = compiledAuctionValidator
    }

-- | Write the blueprint to a file at the given path
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = do
  putStrLn $ "Writing blueprint to: " <> path
  writeBlueprint path myContractBlueprint
  putStrLn "Blueprint successfully written."

-- | Main entry point
main :: IO ()
main = getArgs >>= \case
  [filePath] -> writeBlueprintToFile filePath
  args       -> do
    hPutStrLn stderr $ "Expected exactly one argument (output file path), but got: " <> show args
    die "Usage: auction-blueprint <output-file-path>"
