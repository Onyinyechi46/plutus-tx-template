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

-- | Hardcoded values (replace with actual values as needed)
sellerPkhHex :: BuiltinByteString
sellerPkhHex = stringToBuiltinByteStringHex
  "0000000000000000000000000000000000000000\
  \0000000000000000000000000000000000000000"

currencySymbolHex :: BuiltinByteString
currencySymbolHex = stringToBuiltinByteStringHex
  "00000000000000000000000000000000000000000000000000000000"

-- | Auction parameters (customize for your use case)
auctionParams :: AuctionParams
auctionParams = AuctionParams
  { apSeller          = Crypto.PubKeyHash sellerPkhHex
  , apCurrencySymbol  = Value.CurrencySymbol currencySymbolHex
  , apTokenName       = Value.tokenName "MY_TOKEN"
  , apMinBid          = 100  -- In lovelace
  , apEndTime         = Time.fromMilliSeconds 1_725_227_091_000
  }

-- | Main contract blueprint
myContractBlueprint :: ContractBlueprint
myContractBlueprint = MkContractBlueprint
  { contractId           = Just "auction-validator"
  , contractPreamble     = myPreamble
  , contractValidators   = Set.singleton myValidator
  , contractDefinitions  =
      deriveDefinitions @[AuctionParams, AuctionDatum, AuctionRedeemer]
  }

-- | Metadata for the contract
myPreamble :: Preamble
myPreamble = MkPreamble
  { preambleTitle         = "Auction Validator"
  , preambleDescription   = Just "Blueprint for a Plutus script validating auction transactions"
  , preambleVersion       = "1.0.0"
  , preamblePlutusVersion = PlutusV2
  , preambleLicense       = Just "MIT"
  }

-- | Validator definition
myValidator :: ValidatorBlueprint referencedTypes
myValidator = MkValidatorBlueprint
  { validatorTitle       = "Auction Validator"
  , validatorDescription = Just "Plutus script validating auction transactions"
  , validatorParameters  =
      [ MkParameterBlueprint
          { parameterTitle       = Just "Parameters"
          , parameterDescription = Just "Compile-time validator parameters"
          , parameterPurpose     = Set.singleton Spend
          , parameterSchema      = definitionRef @AuctionParams
          }
      ]
  , validatorRedeemer = MkArgumentBlueprint
      { argumentTitle       = Just "Redeemer"
      , argumentDescription = Just "Redeemer for the auction validator"
      , argumentPurpose     = Set.fromList [Spend]
      , argumentSchema      = definitionRef @()
      }
  , validatorDatum = Nothing
  , validatorCompiled = Just $
      let script = auctionValidatorScript auctionParams
          code   = Short.fromShort (serialiseCompiledCode script)
       in compiledValidator PlutusV2 code
  }

-- | Write the contract blueprint to the specified file
writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile = flip writeBlueprint myContractBlueprint

-- | Entry point
main :: IO ()
main = getArgs >>= \case
  [filePath] -> writeBlueprintToFile filePath
  args       -> fail $
    "Expected exactly one argument (output file path), but got: " <> show args
