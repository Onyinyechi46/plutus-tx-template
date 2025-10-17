{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import AuctionValidator
import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V1.Address (Address(..))

-- | Arbitrary instance for Bid (ensures realistic and valid values)
instance Arbitrary Bid where
    arbitrary = do
        addr <- pure $ Address (PubKeyCredential (PubKeyHash "dummyPKH")) Nothing
        pkh  <- pure $ PubKeyHash "bidderPKH"
        amt  <- arbitrary `suchThat` (> 0)  -- Only positive amounts
        return $ Bid addr pkh amt

-- | Property: A new bid should be accepted only if it is higher than the previous one.
property_newBidHigherThanPrevious :: Bid -> Bid -> Property
property_newBidHigherThanPrevious previousBid newBid =
    let params   = AuctionParams
            { apSeller        = "sellerPKH"
            , apCurrencySymbol = "currencySymbol"
            , apTokenName     = "MY_TOKEN"
            , apMinBid        = 100
            , apDeadline      = 1725227091000
            }
        datum    = AuctionDatum (Just previousBid)
        redeemer = NewBid newBid
        context  = mockScriptContext
    in (bAmount newBid > bAmount previousBid) ==>
        auctionTypedValidator params datum redeemer context

-- | Run QuickCheck property
main :: IO ()
main = do
    putStrLn "🔍 Running Auction Validator Property Test..."
    quickCheck property_newBidHigherThanPrevious
