module Order where

import Types
import Market

-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

-- algorithm of my makeOrders function:
-- step 1: check what to sell
-- step 2: check what to short-sell
-- step 3: check what to buy
-- rules:
-- 1. keep 50% of the wealth as cash
-- 2. only 10% of wealth coming from short-sell
makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders po@(cash, _) history
    | (length $ snd $ head history) < 5 = []
    | cash < 0.2 * calculateWealth po history = []
    | otherwise = makeSellOrder po history ++ makeShortSellOrder po history
    ++ makeShortSellBuyBack po history
    ++ makeBuyOrder po history

     where
         makeSellOrder :: Portfolio -> [StockHistory] -> [Order]
         makeSellOrder po history = case po of
             (_, []) -> []
             (_, x:xs)
                 | (getStockPricex 0 x history - getStockPricex 4 x history) / (getStockPricex 0 x history)  < (-0.05) -> [Order (fst x) (-snd x)] ++ makeSellOrder (fst po, xs) history
                 | otherwise -> makeSellOrder (fst po, xs) history

             where
                   getStockPricex :: Int -> Holding -> [StockHistory] -> Price
                   getStockPricex index s histories = (snd getStock) !! index
                       where
                           getStock = head $ filter (\x -> fst x == fst s) histories

         makeShortSellOrder :: Portfolio -> [StockHistory] -> [Order]
         makeShortSellOrder po@(cash, _) history = case history of
             []   -> []
             (s,p):xs
                 | head p < p !! 1 &&  p!!1 < p !! 2 && p !! 2 < p !! 3 -> [Order s (floor (cash * 0.02 / head p))] ++ makeShortSellOrder po xs
                 | otherwise -> makeShortSellOrder po xs

         makeShortSellBuyBack :: Portfolio -> [StockHistory] -> [Order]
         makeShortSellBuyBack po history = case po of
            (_, []) -> []
            (_, x:xs)
                 | snd x < 0 && ((snd $ head $ filter (\y -> fst y == fst x) history)!!0 - (snd $ head $ filter (\y -> fst y == fst x) history)!! 4)/ (snd $ head $ filter (\y -> fst y == fst x) history)!!0 < (-0.10) -> [Order (fst x) (-snd x) ] ++ makeShortSellBuyBack (fst po, xs) history
                 | otherwise -> makeShortSellBuyBack (fst po, xs) history

         makeBuyOrder :: Portfolio -> [StockHistory] -> [Order]
         makeBuyOrder po history = case history of
              []   -> []
              (s,p):xs
                   | (head p -  p!!4)/ (head p) > 0.05 -> [Order s (floor (cash / 19 / head p))] ++ makeBuyOrder po xs
                   | otherwise -> makeBuyOrder po xs







