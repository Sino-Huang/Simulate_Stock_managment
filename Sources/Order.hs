module Order where

import Types


-- Change this implementation to your own non-trivial trading strategy.
-- Do not modify the type signature of the function.
--

-- algorithm of my makeOrders function:
-- step 1: check what to sell
-- step 2: check what to short-sell and short-sell buy back
-- step 3: check what to buy
-- rules:
-- 1. try to keep cash more than 0
-- 2. When the market condition is bad, try to sell more stocks

makeOrders :: Portfolio -> [StockHistory] -> [Order]
makeOrders po@(cash, _) history
-- use 4 days to have a trend of the stocks
    | (length $ snd $ head history) < 5 = []
-- try to keep cash more than 0, thus only execute makeSellOrder and makeShortSellBuyBack
    | cash < 0 = makeSellOrder po history ++ makeShortSellBuyBack po history
-- when the market condition is bad, the conditions imposed on buying in were stringent, while the conditions on sell out were loosen
    | marketCondition history == 1 =  makeSellOrderAtBadTime po history ++ makeShortSellOrder 2 po history ++ makeShortSellBuyBack po history ++ makeBuyOrderAtBadTime po history
    | otherwise = makeSellOrder po history ++ makeShortSellOrder 4 po history ++ makeShortSellBuyBack po history ++ makeBuyOrder po history

     where
         makeSellOrder :: Portfolio -> [StockHistory] -> [Order]
         makeSellOrder p h = case p of
             (_, []) -> []
             (_, x:xs)
-- Sell out the stocks when it drop more than 0.16 in ratio within 4 days
                 | (getStockPricex 0 x h - getStockPricex 4 x h) / (getStockPricex 0 x h) < (-0.16)  && snd x > 0 -> [Order (fst x) (-snd x)] ++ makeSellOrder (fst p, xs) h
                 | otherwise -> makeSellOrder (fst p, xs) h

             where
                   getStockPricex :: Int -> Holding -> [StockHistory] -> Price
                   getStockPricex index s hs = (snd getStock) !! index
                       where
                           getStock = head $ filter (\x -> fst x == fst s) hs

-- At bad time, sell out the stocks when it drop more than 0.1 in ratio within 4 days
         makeSellOrderAtBadTime:: Portfolio -> [StockHistory] -> [Order]
         makeSellOrderAtBadTime p h = case p of
             (_, []) -> []
             (_, x:xs)
                  | (getStockPricex 0 x h - getStockPricex 4 x h) / (getStockPricex 0 x h)  < (- 0.1)  && snd x > 0 -> [Order (fst x) (-snd x)] ++ makeSellOrderAtBadTime (fst p, xs) h
                  | otherwise -> makeSellOrderAtBadTime (fst p, xs) h

             where
                   getStockPricex :: Int -> Holding -> [StockHistory] -> Price
                   getStockPricex index s hs = (snd getStock) !! index
                       where
                           getStock = head $ filter (\x -> fst x == fst s) hs

-- Short sell the stocks when it continues dropping within x days
         makeShortSellOrder :: Int -> Portfolio -> [StockHistory] -> [Order]
         makeShortSellOrder num port@(cashs, _) h = case h of
             []   -> []
             (s,p):xs
                 | keyDay p num && not (elem s (map fst (snd port)))  -> [Order s (abs(floor (cashs * 0.2 / head p)))] ++ makeShortSellOrder num port xs
                 | otherwise -> makeShortSellOrder num port xs
             where
                 keyDay :: [Price] -> Int -> Bool
                 keyDay p x
                     | x <= 1 = p !! 1 > p !! 0
                     | otherwise = p !! x > p !!(x-1) && keyDay p (x-1)

-- Short sell buy back when the short-sell stock drop 0.16 in ratio within 4 days
         makeShortSellBuyBack :: Portfolio -> [StockHistory] -> [Order]
         makeShortSellBuyBack p h = case p of
            (_, []) -> []
            (_, x:xs)
                 | snd x < 0 && ((snd $ head $ filter (\y -> fst y == fst x) h)!!0 - (snd $ head $ filter (\y -> fst y == fst x) h)!! 4)/ (snd $ head $ filter (\y -> fst y == fst x) h)!!0 < (-0.16) -> [Order (fst x) (-snd x) ] ++ makeShortSellBuyBack (fst p, xs) h
                 | otherwise -> makeShortSellBuyBack (fst p, xs) h

-- Buy in stocks when it's price increase in 0.11 in ratio within 4 days
         makeBuyOrder :: Portfolio -> [StockHistory] -> [Order]
         makeBuyOrder port@(cashs, _) h = case h of
              []   -> []
              (s,p):xs
                   | (head p -  p!!4)/ (head p) > 0.11 -> [Order s (abs (floor (cashs / 4 / head p)))] ++ makeBuyOrder port xs
                   | otherwise -> makeBuyOrder port xs

-- At bad time, only buy in the stocks when the price continues increasing and increase in 0.11 in ratio within 4 days
         makeBuyOrderAtBadTime :: Portfolio -> [StockHistory] -> [Order]
         makeBuyOrderAtBadTime port@(cashs, _) h = case h of
              []   -> []
              (s,p):xs
                   | (head p -  p!!4)/ (head p) > 0.11 && (keyDay' p 4) -> [Order s (abs (floor (cashs / 4 / head p)))] ++ makeBuyOrderAtBadTime port xs
                   | otherwise -> makeBuyOrderAtBadTime port xs
              where
                 keyDay' :: [Price] -> Int -> Bool
                 keyDay' p x
                     | x <= 1 = p !! 1 < p !! 0
                     | otherwise = p !! x < p !!(x-1) && keyDay' p (x-1)


-- Check market condition, 0 for good, 1 for bad, it is bad when 80 percent of the stocks continue dropping in 2 days.
         marketCondition :: [StockHistory] -> Integer
         marketCondition h
             | ((fromIntegral $ length(filter (\x -> x == True) (map whetherContinueDrop (map snd h)))) / (fromIntegral $ length((map whetherContinueDrop (map snd h))))) > 0.8 = 1
             | otherwise = 0

             where
                 whetherContinueDrop :: [Price] -> Bool
                 whetherContinueDrop p
                     | p !! 2 > p !! 1 && p !! 1 > p !! 0 = True
                     | otherwise = False






