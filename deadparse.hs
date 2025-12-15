import Data.Char

thd :: (a,b,c) -> c
thd (_,_,x) = x

mySnd :: (a,b,c) -> b
mySnd (_,x,_) = x

myFst :: (a,b,c) -> a
myFst (x,_,_) = x

parse code = reverse $ myFst $ go (filter (not . isSpace) code) 0 [] []

go "" state output funtions = (output,state,funtions)
go [x] state output functions
  |x == 'x' = (output,state,functions)
  |x == 'k' = (intToDigit state : output,state,functions)
  |x == 'c' = (output,state,functions)
  |x == 'd' = (output,state,functions)
  |x == 'X' = error "function defined without name!"
  |x == 'K' = (chr state:output,state,functions)
  |x == 'C' = error "function called without name"
  |x == 'D' = (output,state,functions)
go (x:xs) state output functions
  | x == 'x' = go xs (cor ((state + 1) `mod` 256)) output functions
  | x == 'k' = go xs state (intToDigit state : output) functions
  | x == 'c' = go xs (cor (state * state)) output functions
  | x == 'd' = go xs (cor (state - 1)) output functions
  | x == 'X' = go (tail (dropWhile (/= 'C') xs)) state output (newFunc (takeWhile (/= 'C') xs) functions)
  | x == 'K' = go xs state (chr state : output) functions
  | x == 'C' = let
    func = case lookup (head xs) functions of
      Nothing -> error "function not found"
      Just b  -> b
    result = go func state output functions in
    go (tail xs) (mySnd result) (myFst result) (thd result)
  | x == 'D' = go xs 0 output functions
  | x `elem` map fst functions = error "Incorrect Function call"
  | otherwise = error "Syntax error!"

newFunc "" ls = error "Function defined with no name"
newFunc "a" ls = error "Function defined with no code"
newFunc (x:xs) ls = (x,xs) : ls

cor x = if x >= 0 && x < 256 then x else 0

main = print $ parse "XUxKCxxcxxxxcCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCUCU"