{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import AuctionValidator

-- | Arbitrary instance for Bid
instance Arbitrary Bid where
    arbitrary = do
        addr <- arbitrary
        pkh <- arbitrary
        amt <- arbitrary `suchThat` (> 0)  -- Ensure the bid amount is positive
        return $ Bid addr pkh amt

-- | Mock AuctionParams for tests
mockAuctionParams :: AuctionParams
mockAuctionParams = AuctionParams
    { aSeller        = "seller"
    , aCurrencySymbol = "currencySymbol"
    , aTokenName     = "MY_TOKEN"
    , aMinBid        = 100
    , aDeadline      = 1725227091000
    }

-- | Property: new bid must be higher than the previous bid
property_newBidHigherThanPrevious :: Bid -> Bid -> Property
property_newBidHigherThanPrevious previousBid newBid =
    let datum    = AuctionDatum (Just previousBid)
        redeemer = NewBid newBid
        context  = mockScriptContext
    in (bAmount newBid > bAmount previousBid) ==>
        auctionTypedValidator mockAuctionParams datum redeemer context

-- | Property: bids lower or equal to the previous should fail
property_lowerBidFails :: Bid -> Bid -> Property
property_lowerBidFails previousBid newBid =
    let datum    = AuctionDatum (Just previousBid)
        redeemer = NewBid newBid
        context  = mockScriptContext
    in (bAmount newBid <= bAmount previousBid) ==>
        not (auctionTypedValidator mockAuctionParams datum redeemer context)

main :: IO ()
main = do
    putStrLn "\n=== Auction Validator Property Tests ==="
    quickCheck property_newBidHigherThanPrevious
    quickCheck property_lowerBidFails
