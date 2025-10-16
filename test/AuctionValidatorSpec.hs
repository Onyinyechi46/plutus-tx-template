{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Test.Hspec
import           AuctionValidator

import           PlutusLedgerApi.V1             (Lovelace (..))
import           PlutusLedgerApi.V1.Crypto      (PubKeyHash (..))
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

-- | Helper to construct a dummy PubKeyHash
mkPubKeyHash :: BuiltinByteString -> PubKeyHash
mkPubKeyHash = PubKeyHash

-- | Helper to create a bid
mkBid :: BuiltinByteString -> BuiltinByteString -> Integer -> Bid
mkBid addr pkh amount = Bid addr (mkPubKeyHash pkh) (Lovelace amount)

-- | Helper to construct dummy auction parameters
mkAuctionParams :: AuctionParams
mkAuctionParams = AuctionParams
  { apSeller         = mkPubKeyHash "12345678"
  , apCurrencySymbol = CurrencySymbol ""    -- dummy
  , apTokenName      = TokenName "MY_TOKEN"
  , apMinBid         = Lovelace 100
  , apEndTime        = 1725227091000
  }

-- | An empty/dummy ScriptContext used for unit testing
mockScriptContext :: ScriptContext
mockScriptContext =
  ScriptContext
    { scriptContextTxInfo = TxInfo
        { txInfoInputs           = []
        , txInfoReferenceInputs  = []
        , txInfoOutputs          = []
        , txInfoFee              = mempty
        , txInfoMint             = mempty
        , txInfoDCert            = []
        , txInfoWdrl             = AssocMap.empty
        , txInfoValidRange       = always
        , txInfoSignatories      = []
        , txInfoData             = AssocMap.empty
        , txInfoId               = TxId ""
        , txInfoRedeemers        = AssocMap.empty
        }
    , scriptContextPurpose = Spending (TxOutRef (TxId "") 0)
    }

main :: IO ()
main = hspec $ do
  describe "auctionTypedValidator" $ do
    it "rejects a new bid when the context has no outputs" $ do
      let previousBid = Just $ mkBid "addr" "oldBidder" 50
          newBid      =        mkBid "addr" "newBidder" 150
          datum       = AuctionDatum previousBid
          redeemer    = NewBid newBid

      auctionTypedValidator mkAuctionParams datum redeemer mockScriptContext
        `shouldBe` False
