{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import           AuctionValidator
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import qualified PlutusLedgerApi.V1.Crypto   as Crypto
import qualified PlutusLedgerApi.V1.Time     as Time
import qualified PlutusLedgerApi.V1.Value    as Value
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import           System.Environment          (getArgs)
import           System.Exit                 (die)
import           Control.Monad               (unless)

-- ================================================================
-- | Auction Configuration
--   Define your auction parameters here.
-- ================================================================

-- | Create default auction parameters.
mkAuctionParams :: AuctionParams
mkAuctionParams = AuctionParams
  { apSeller          = Crypto.PubKeyHash $ hex "00000000000000000000000000000000000000000000000000000000"
  , apCurrencySymbol  = Value.CurrencySymbol $ hex "00000000000000000000000000000000000000000000000000000000"
  , apTokenName       = Value.tokenName "MY_TOKEN"
  , apMinBid          = 100                     -- Minimum bid in lovelace
  , apEndTime         = Time.fromMilliSeconds 1_725_227_091_000
  }
  where
    hex = stringToBuiltinByteStringHex

auctionParams :: AuctionParams
auctionParams = mkAuctionParams

-- ================================================================
-- | Blueprint Definition
-- ================================================================
myContractBlueprint :: ContractBlueprint
myContractBlueprint = MkContractBlueprint
  { contractId          = Just "auction-validator"
  , contractPreamble    = myPreamble
  , contractValidators  = Set.singleton myValidator
  , contractDefinitions =
      deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
  }

-- ================================================================
-- | Preamble Metadata
-- ================================================================
myPreamble :: Preamble
myPreamble = MkPreamble
  { preambleTitle         = "Auction Validator"
  , preambleDescription   = Just "Blueprint for a Plutus script validating auction transactions"
  , preambleVersion       = "1.0.0"
  , preamblePlutusVersion = PlutusV2
  , preambleLicense       = Just "MIT"
  }

-- ================================================================
-- | Validator Blueprint Definition
-- ================================================================
myValidator :: ValidatorBlueprint referencedTypes
myValidator = MkValidatorBlueprint
  { validatorTitle        = "Auction Validator"
  , validatorDescription  = Just "Plutus script for validating auction transactions"
  , validatorParameters   =
      [ MkParameterBlueprint
          { parameterTitle       = Just "Auction Parameters"
          , parameterDescription = Just "Compile-time parameters for the auction validator"
          , parameterPurpose     = Set.singleton Spend
          , parameterSchema      = definitionRef @AuctionParams
          }
      ]
  , validatorRedeemer =
      MkArgumentBlueprint
        { argumentTitle       = Just "Redeemer"
        , argumentDescription = Just "Redeemer for the auction validator"
        , argumentPurpose     = Set.singleton Spend
        , argumentSchema      = definitionRef @()
        }
  , validatorDatum     = Nothing
  , validatorCompiled  = compileAuctionValidator
  }

-- ================================================================
-- | Compile & Serialize Validator
-- ================================================================
compileAuctionValidator :: Maybe CompiledValidator
compileAuctionValidator =
  let script = auctionValidatorScript auctionParams
      code   = Short.fromShort (serialiseCompiledCode script)
  in Just (compiledValidator PlutusV2 code)

-- ================================================================
-- | Write Blueprint to JSON File
-- ================================================================
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = do
  putStrLn $ "💾 Writing blueprint to file: " <> path
  writeBlueprint path myContractBlueprint
  putStrLn "✅ Blueprint successfully written."

-- ================================================================
-- | Entry Point
-- ================================================================
main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputPath] -> writeBlueprintToFile outputPath
    _ -> die usageMsg
  where
    usageMsg =
      unlines
        [ "❌ Invalid arguments."
        , "Usage: runhaskell Main.hs <output-file-path>"
        , "Example: runhaskell Main.hs ./auction-blueprint.json"
        ]
