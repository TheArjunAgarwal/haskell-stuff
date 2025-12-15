import Data.Char (digitToInt)
eqRiseFall :: [Int] -> Bool
eqRiseFall l = sum (map (\x -> if x < 0 then -1 else if x > 0 then 1 else 0) (zipWith (-) l (tail l) )) == 0


isEsthetic :: Int -> Bool
isEsthetic n = eqRiseFall $ map digitToInt $ show n

a296712 = [n | n <- [1..], isEsthetic n]

sol1 = take 200 a296712
sol2 = a296712 !! 9999999

-- Print the first   200   numbers in the sequence
--   Show that the   10 millionth   (10,000,000th)   number in the sequence is   41,909,002

