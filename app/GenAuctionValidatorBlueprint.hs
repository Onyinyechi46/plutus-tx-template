{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

-- Imports
import           AuctionValidator                -- Your custom module
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1.Crypto   as Crypto
import qualified PlutusLedgerApi.V1.Time     as Time
import qualified PlutusLedgerApi.V1.Value    as Value
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import           System.Environment          (getArgs)

-- Auction Parameters (Replace placeholders with actual values)
auctionParams :: AuctionParams
auctionParams =
  AuctionParams
    { apSeller =
        Crypto.PubKeyHash $
          stringToBuiltinByteStringHex
            "0000000000000000000000000000000000000000000000000000000000000000" -- TODO: Replace with real PubKeyHash
    , apCurrencySymbol =
        Value.CurrencySymbol $
          stringToBuiltinByteStringHex
            "00000000000000000000000000000000000000000000000000000000" -- TODO: Replace with real currency symbol
    , apTokenName =
        Value.tokenName "MY_TOKEN" -- TODO: Replace with real token name
    , apMinBid = 100              -- Minimal bid in lovelace
    , apEndTime = Time.fromMilliSeconds 1_725_227_091_000 -- TODO: Replace with desired end time
    }

-- Contract Blueprint Definition
myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId           = Just "auction-validator"
    , contractPreamble     = auctionPreamble
    , contractValidators   = Set.singleton auctionValidatorBlueprint
    , contractDefinitions  = deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
    }

-- Preamble (Metadata)
auctionPreamble :: Preamble
auctionPreamble =
  MkPreamble
    { preambleTitle        = "Auction Validator"
    , preambleDescription  = Just "Blueprint for a Plutus script validating auction transactions"
    , preambleVersion      = "1.0.0"
    , preamblePlutusVersion= PlutusV2
    , preambleLicense      = Just "MIT"
    }

-- Validator Definition
auctionValidatorBlueprint :: ValidatorBlueprint referencedTypes
auctionValidatorBlueprint =
  MkValidatorBlueprint
    { validatorTitle        = "Auction Validator"
    , validatorDescription  = Just "Plutus script validating auction transactions"
    , validatorParameters   =
        [ MkParameterBlueprint
            { parameterTitle       = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose     = Set.singleton Spend
            , parameterSchema      = definitionRef @AuctionParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle         = Just "Redeemer"
          , argumentDescription   = Just "Redeemer for the auction validator"
          , argumentPurpose       = Set.singleton Spend
          , argumentSchema        = definitionRef @()
          }
    , validatorDatum       = Nothing
    , validatorCompiled    =
        let compiled = auctionValidatorScript auctionParams
            code     = Short.fromShort $ serialiseCompiledCode compiled
        in Just $ compiledValidator PlutusV2 code
    }

-- Write the blueprint to a file
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

-- Entry Point
main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args  -> fail $ "❌ Expected exactly one argument (output file path), but got: " <> show (length args)
