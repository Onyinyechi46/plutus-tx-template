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
                                               , TxOut          (..)
                                               , Value
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

-- | Dummy bidders for test readability
oldBidder, newBidder :: PubKeyHash
oldBidder = PubKeyHash "oldBidder"
newBidder = PubKeyHash "newBidder"

-- | Helper: Create a simple TxOut holding the token and a given amount of lovelace
mkTxOut :: PubKeyHash -> Lovelace -> CurrencySymbol -> TokenName -> TxOut
mkTxOut pkh (Lovelace lovelaceAmt) cs tn =
  TxOut
    { txOutAddress = Address (PubKeyCredential pkh) Nothing
    , txOutValue = singleton cs tn 1 <> lovelaceValueOf lovelaceAmt
    , txOutDatumHash = Nothing
    }

-- | Construct a singleton Value for the token
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleton = -- Implementation depends on your environment; usually comes from Plutus libraries

-- | lovelaceValueOf helper from Ledger.Api.V1.Value
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = -- Implementation depends on your environment; usually comes from Plutus libraries

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

    it "accepts a higher bid when the context has a valid output for the new bidder" $ do
      let params      = mkParams (PubKeyHash "12345678") (Lovelace 100)
          previousBid = Just (Bid "addr" oldBidder (Lovelace 100))
          newBid      = Bid   "addr" newBidder (Lovelace 200)
          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid

          -- Create a TxOut representing the output to the new bidder holding the token
          txOutNewBidder = mkTxOut newBidder (Lovelace 200) (apCurrencySymbol params) (apTokenName params)

          ctx = mockScriptContext
                { scriptContextTxInfo = (scriptContextTxInfo mockScriptContext)
                    { txInfoOutputs = [txOutNewBidder]
                    }
                }

      auctionTypedValidator params datum redeemer ctx
        `shouldBe` True

