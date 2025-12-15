trueMerge :: (a-> a -> Bool) -> [a] -> [a] -> [a]
trueMerge _ xs [] = xs
trueMerge _ [] ys = ys
trueMerge f (x:xs) (y:ys) = if f x y then x : (trueMerge f xs (y:ys)) else y : (trueMerge f (x:xs) ys)

trueSort ::  (a -> a -> Bool) -> [a] -> [a]
trueSort _ [] = []
trueSort _ [a] = [a]
trueSort f xs = trueMerge f (trueSort f left) (trueSort f right) where
  (left, right) = ((take n xs), (drop n xs))
  n = (length xs) `div` 2

decSorUndec :: Ord b => (a -> b) -> [a] -> [a]
decSorUndec _ [] = []
decSorUndec _ [x] = [x]
decSorUndec f xs = map fst $ trueSort (\(_,y1) (_,y2) -> y1 < y2 ) $ map (\x -> (x, f x)) xs

main = print $ decSorUndec length ["helloing", "gamer", "joke", "a", "ab"]