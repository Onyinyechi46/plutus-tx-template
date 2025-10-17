{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import AuctionValidator

-- Arbitrary instance for Bid with positive amounts
instance Arbitrary Bid where
    arbitrary = do
        addr <- arbitrary
        pkh <- arbitrary
        amt <- arbitrary `suchThat` (> 0)  -- Positive bid amount
        return $ Bid addr pkh amt

-- Mock AuctionParams for testing
mockAuctionParams :: AuctionParams
mockAuctionParams = AuctionParams
    { aSeller        = "seller"
    , aCurrencySymbol = "currencySymbol"
    , aTokenName     = "MY_TOKEN"
    , aMinBid        = 100
    , aDeadline      = 1725227091000
    }

-- Mock ScriptContext placeholder
mockScriptContext :: ScriptContext
mockScriptContext = undefined  -- You need a real or mocked ScriptContext here

-- Property: New bid must be strictly higher than the previous bid
property_newBidHigherThanPrevious :: Property
property_newBidHigherThanPrevious = 
    forAll arbitrary $ \previousBid ->
    forAll arbitrary $ \newBid ->
    (bAmount newBid > bAmount previousBid) ==>
        let datum    = AuctionDatum (Just previousBid)
            redeemer = NewBid newBid
            context  = mockScriptContext
            result   = auctionTypedValidator mockAuctionParams datum redeemer context
        in counterexample
            ("Previous bid: " ++ show (bAmount previousBid) ++
             ", New bid: " ++ show (bAmount newBid) ++
             ", Validator result: " ++ show result)
            result

-- Property: Bids that are lower or equal to the previous bid should fail validation
property_lowerOrEqualBidFails :: Property
property_lowerOrEqualBidFails = 
    forAll arbitrary $ \previousBid ->
    forAll arbitrary $ \newBid ->
    (bAmount newBid <= bAmount previousBid) ==>
        let datum    = AuctionDatum (Just previousBid)
            redeemer = NewBid newBid
            context  = mockScriptContext
            result   = auctionTypedValidator mockAuctionParams datum redeemer context
        in counterexample
            ("Previous bid: " ++ show (bAmount previousBid) ++
             ", New bid: " ++ show (bAmount newBid) ++
             ", Validator result: " ++ show result)
            (not result)

main :: IO ()
main = do
    putStrLn "\n=== Auction Validator Property Tests ==="
    quickCheck $ label "New bids higher than previous should validate" property_newBidHigherThanPrevious
    quickCheck $ label "Bids lower or equal to previous should fail" property_lowerOrEqualBidFails
