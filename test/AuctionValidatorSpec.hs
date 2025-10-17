{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Test.Hspec
import           AuctionValidator

import           PlutusLedgerApi.V1.Crypto      (PubKeyHash (..))
import           PlutusLedgerApi.V1             (Lovelace (..))
import           PlutusLedgerApi.V1.Interval    (always)

import           PlutusLedgerApi.V2             ( CurrencySymbol (..)
                                               , TokenName      (..)
                                               , ScriptContext  (..)
                                               , TxInfo         (..)
                                               )
import           PlutusLedgerApi.V2.Contexts    ( ScriptPurpose (..)
                                               , TxOutRef       (..)
                                               , TxId           (..)
                                               )
import qualified PlutusTx.AssocMap             as AssocMap

-- | Minimal mock ScriptContext for unit testing.
mockScriptContext :: ScriptContext
mockScriptContext =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs          = []
          , txInfoReferenceInputs = []
          , txInfoOutputs         = []
          , txInfoFee             = mempty
          , txInfoMint            = mempty
          , txInfoDCert           = []
          , txInfoWdrl            = AssocMap.empty
          , txInfoValidRange      = always
          , txInfoSignatories     = []
          , txInfoData            = AssocMap.empty
          , txInfoId              = TxId ""
          , txInfoRedeemers       = AssocMap.empty
          }
    , scriptContextPurpose = Spending (TxOutRef (TxId "") 0)
    }

-- | Helper: construct dummy AuctionParams quickly
mkParams :: PubKeyHash -> Lovelace -> AuctionParams
mkParams seller minBid =
  AuctionParams
    { apSeller         = seller
    , apCurrencySymbol = CurrencySymbol ""
    , apTokenName      = TokenName "MY_TOKEN"
    , apMinBid         = minBid
    , apEndTime        = 1725227091000
    }

-- | Dummy bids for test readability
oldBidder, newBidder :: PubKeyHash
oldBidder = PubKeyHash "oldBidder"
newBidder = PubKeyHash "newBidder"

-- | Main test suite
main :: IO ()
main = hspec $ do
  describe "auctionTypedValidator" $ do

    it "rejects a new bid when the context has no outputs" $ do
      let params      = mkParams (PubKeyHash "12345678") (Lovelace 100)
          previousBid = Just (Bid "addr" oldBidder (Lovelace 50))
          newBid      = Bid   "addr" newBidder (Lovelace 150)
          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid
          ctx         = mockScriptContext

      auctionTypedValidator params datum redeemer ctx
        `shouldBe` False

    it "accepts a higher bid when context is valid (placeholder test)" $ do
      let params      = mkParams (PubKeyHash "12345678") (Lovelace 100)
          previousBid = Just (Bid "addr" oldBidder (Lovelace 100))
          newBid      = Bid   "addr" newBidder (Lovelace 200)
          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid
          ctx         = mockScriptContext { scriptContextTxInfo = (scriptContextTxInfo mockScriptContext) { txInfoOutputs = [] } }
          -- TODO: Replace [] with a valid TxOut simulating the new bidder’s output

      auctionTypedValidator params datum redeemer ctx
        `shouldBe` False  -- Change to True when TxOuts are mocked properly
