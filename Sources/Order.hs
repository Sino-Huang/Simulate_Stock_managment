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
--    | cash < 0.1 * calculateWealth po history = makeSellOrder po history ++ makeShortSellBuyBack po history
    | marketCondition history == 1 = sellThemAll po ++ makeShortSellOrder po history ++ makeShortSellBuyBack po history
    | otherwise = makeSellOrder po history ++ makeShortSellOrder po history ++ makeShortSellBuyBack po history ++ makeBuyOrder po history

     where
         makeSellOrder :: Portfolio -> [StockHistory] -> [Order]
         makeSellOrder po history = case po of
             (_, []) -> []
             (_, x:xs)
                 | (getStockPricex 0 x history - getStockPricex 4 x history) / (getStockPricex 0 x history)  < (-0.15) -> [Order (fst x) (-snd x)] ++ makeSellOrder (fst po, xs) history
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
                 | keyDay p 2 -> [Order s (floor (cash * 0.02 / head p))] ++ makeShortSellOrder po xs
                 | otherwise -> makeShortSellOrder po xs
             where
                 keyDay :: [Price] -> Int -> Bool
                 keyDay p x
                     | x <= 1 = p !! 1 > p !! 0
                     | otherwise = p !! x > p !!(x-1) && keyDay p (x-1)

         makeShortSellBuyBack :: Portfolio -> [StockHistory] -> [Order]
         makeShortSellBuyBack po history = case po of
            (_, []) -> []
            (_, x:xs)
                 | snd x < 0 && ((snd $ head $ filter (\y -> fst y == fst x) history)!!0 - (snd $ head $ filter (\y -> fst y == fst x) history)!! 4)/ (snd $ head $ filter (\y -> fst y == fst x) history)!!0 < (-0.15) -> [Order (fst x) (-snd x) ] ++ makeShortSellBuyBack (fst po, xs) history
                 | otherwise -> makeShortSellBuyBack (fst po, xs) history

         makeBuyOrder :: Portfolio -> [StockHistory] -> [Order]
         makeBuyOrder po history = case history of
              []   -> []
              (s,p):xs
                   | (head p -  p!!4)/ (head p) > 0.11 -> [Order s (floor (cash / 3 / head p))] ++ makeBuyOrder po xs
                   | otherwise -> makeBuyOrder po xs

-- 0 for good, 1 for bad
         marketCondition :: [StockHistory] -> Integer
         marketCondition history
             | ((fromIntegral $ length(filter (\x -> x == True) (map whetherContinueDrop (map snd history)))) / (fromIntegral $ length((map whetherContinueDrop (map snd history))))) > 0.8 = 1
             | otherwise = 0

             where
                 whetherContinueDrop :: [Price] -> Bool
                 whetherContinueDrop p
                     | p !! 2 > p !! 1 && p !! 1 > p !! 0 = True
                     | otherwise = False

         sellThemAll :: Portfolio -> [Order]
         sellThemAll po@(cash,holding) = case holding of
             [] -> []
             (s, q):xs
                 | q > 0 -> [Order s (-q)] ++ sellThemAll (cash, xs)
                 | otherwise -> sellThemAll (cash, xs)







