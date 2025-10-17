{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           Test.Hspec
import           AuctionValidator

import           PlutusLedgerApi.V1.Crypto      (PubKeyHash (..))
import           PlutusLedgerApi.V1             (Lovelace (..))
import           PlutusLedgerApi.V1.Interval    (always)

import           PlutusLedgerApi.V2             ( CurrencySymbol (..)
                                               , TokenName (..)
                                               , ScriptContext (..)
                                               , TxInfo (..)
                                               )
import           PlutusLedgerApi.V2.Contexts    ( ScriptPurpose (..)
                                               , TxOutRef (..)
                                               , TxId (..)
                                               )
import qualified PlutusTx.AssocMap             as AssocMap

------------------------------------------------------------
-- Mock Utilities
------------------------------------------------------------

-- | A minimal empty TxInfo, reusable for testing.
emptyTxInfo :: TxInfo
emptyTxInfo = TxInfo
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

-- | A generic empty ScriptContext.
mockScriptContext :: ScriptContext
mockScriptContext = ScriptContext
  { scriptContextTxInfo = emptyTxInfo
  , scriptContextPurpose = Spending (TxOutRef (TxId "") 0)
  }

------------------------------------------------------------
-- Sample Parameters for Reuse
------------------------------------------------------------

defaultAuctionParams :: AuctionParams
defaultAuctionParams = AuctionParams
  { apSeller         = PubKeyHash "12345678"
  , apCurrencySymbol = CurrencySymbol ""
  , apTokenName      = TokenName "MY_TOKEN"
  , apMinBid         = Lovelace 100
  , apEndTime        = 1725227091000
  }

------------------------------------------------------------
-- Tests
------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "Auction Validator" $ do

    it "rejects a new bid when there are no outputs in the context" $ do
      let previousBid = Just (Bid "addr" (PubKeyHash "oldBidder") (Lovelace 50))
          newBid      = Bid "addr" (PubKeyHash "newBidder") (Lovelace 150)
          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid

      auctionTypedValidator defaultAuctionParams datum redeemer mockScriptContext
        `shouldBe` False
