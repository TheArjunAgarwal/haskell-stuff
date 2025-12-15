-- Byte t={2, 8, 8, 18, 18, 18, 18, 15, 15}; Byte row=0; While(In>t[row]) {In-=t[row]; row++;} return(row++, In)

offset :: [Int]
offset = [2,8,8,18,18,18]

pos :: Int -> (Int, Int)
pos n = go n 0
  where
    go n k
        | k >= length offset = error "Index out of bounds!"  -- Prevents accessing an invalid index
        | n >= offset !! k   = go (n - offset !! k) (k + 1)  -- Continue reducing n
        | otherwise          = (k+1, n)                      -- Found position


-- tableCoord :: Int -> (Int, Int)
-- tableCoord n
--     | n >= 57 && n <= 70 = (8,n-53)
--     | n >= 89 && n <= 102 = (9,n-85)
--     | n == 1  = (1,1)
--     | n == 2 = ()