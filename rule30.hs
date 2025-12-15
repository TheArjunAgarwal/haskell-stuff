import Data.Bits




-- zeroStates = [(1,1,1),(1,1,0),(0,0,1),(0,0,0)]
-- oneStates = [(1,0,1),(0,1,1),(1,0,0),(0,1,0)]

betNext [] = []
betNext [a] = []
betNext [a,b] = [p] where p = a `xor` b
betNext (x:y:z:zs) = (x `xor` (y .|. z)) : betNext (y:z:zs)

-- nextLine [] = []
-- nextLine [a] = []
-- nextLine [a,b]
--   |(a,b,0) `elem` zeroStates = [0]
--   |(a,b,0) `elem` oneStates = [1]
--   |otherwise = error "UpperLine is invalid"
-- nextLine (x:y:z:zs)
--   |(x,y,z) `elem` zeroStates = 0 : nextLine (y:z:zs)
--   |(x,y,z) `elem` oneStates = 1 : nextLine (y:z:zs)
--   |otherwise = error "UpperLine is invalid"

-- generatePattern 1 line = do
--   putStrLn $ map (\x -> if x == 1 then '*' else ' ') line

-- generatePattern n line = do
--   putStrLn $ map (\x -> if x == 1 then '*' else ' ') line
--   let newLine = nextLine (0:line)
--   generatePattern (n-1) newLine


betGeneratePattern 1 line = do
  putStrLn $ map (\x -> if x then '*' else ' ') line

betGeneratePattern n line = do
  putStrLn $ map (\x -> if x then '*' else ' ') line
  let newLine = betNext (False:line)
  betGeneratePattern (n-1) newLine



patternSize n = betGeneratePattern (n+1) [x == n| x<-[0..(2*n)]]


pattern :: Int -> [[Bool]]
pattern n = iterate betNext [x == n| x<-[0..(2*n)]]

list n = map (!! n) (pattern n)

random30 k s = foldl (\acc x -> if x then acc  + acc + 1 else acc + acc) 0 $ drop s $ list (k+s)

main = print $ random30 3 3



