module EDSL where

-- EDSL = Embedded Domain Specific Languages

data Discount = DiscountPercent Int         -- A percentage discount
              | DiscountConstant Int        -- A constant discount
              | MinimumPrice Int            -- Set a minimum price
              | ForCustomer String Discount -- Discounts can be conditional
              | Many [Discount]             -- Apply a number of discounts in row

applyDiscount :: String -> Int -> Discount -> Int
applyDiscount _        price (DiscountPercent percent) = price - (price * percent) `div` 100
applyDiscount _        price (DiscountConstant discount) = price - discount
applyDiscount _        price (MinimumPrice minPrice) = max price minPrice
applyDiscount customer price (ForCustomer target discount)
    | customer == target  = applyDiscount customer price discount
    | otherwise           = price
applyDiscount customer price (Many discounts) = go price discounts
  where go p [] = p
        go p (d:ds) = go (applyDiscount customer p d) ds