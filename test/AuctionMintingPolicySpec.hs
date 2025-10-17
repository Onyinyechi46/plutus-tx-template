{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import AuctionMintingPolicy
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V2.Contexts (ScriptContext(..))
import qualified PlutusLedgerApi.V2 as Plutus

-- | A simple mock of ScriptContext for testing
mockMintingScriptContext :: ScriptContext
mockMintingScriptContext =
    ScriptContext
        { Plutus.scriptContextTxInfo = Plutus.TxInfo
            { Plutus.txInfoInputs = []
            , Plutus.txInfoOutputs = []
            , Plutus.txInfoFee = mempty
            , Plutus.txInfoMint = oneTokenValue
            , Plutus.txInfoDCert = []
            , Plutus.txInfoWdrl = mempty
            , Plutus.txInfoValidRange = Plutus.always
            , Plutus.txInfoSignatories = []
            , Plutus.txInfoRedeemers = mempty
            , Plutus.txInfoData = mempty
            , Plutus.txInfoId = "testTx"
            }
        , Plutus.scriptContextPurpose = Plutus.Minting "testCurrencySymbol"
        }
  where
    oneTokenValue = Plutus.singleton "testCurrencySymbol" "TestToken" 1

main :: IO ()
main = hspec $ do
    describe "Auction Minting Policy" $ do
        it "allows minting exactly one token" $ do
            let pkh = PubKeyHash "12345678"
            let redeemer = ()
            let context = mockMintingScriptContext
            auctionTypedMintingPolicy pkh redeemer context `shouldBe` True
