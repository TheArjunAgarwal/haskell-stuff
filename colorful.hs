import Data.Char (digitToInt)
isColorful :: Int -> Bool
isColorful n
  | n < 0 = error "Only non-negative integers are allowed"
  | n == 0 || n == 1 = True
  | 0 `elem` digits = False
  | 1 `elem` digits = False
  | not (distinct digits) = False
  | not (distinct subpros) = False
  | otherwise = True
  where
    digits = map digitToInt (show n)
    subpros =  [(prods !! i) `div` (prods !! j) |j <- [0..length(digits)], i <- [(j+1)..length(digits)]]
    prods = scanl (*) 1 digits

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct [a] = True
distinct (x:xs) = distinct xs && (x `notElem` xs)


  
sol1 = filter isColorful [0..100]
sol2 = head $ filter isColorful [98762543..]
sol3 = [length (filter isColorful [(10^i)..(10^(i+1))]) | i <- [0..7]]
sol4 = sum sol3