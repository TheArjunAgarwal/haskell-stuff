import Data.Fixed (mod')

special :: Integer -> Integer
special x = go start
  where
    d = length (show x)
    logX = logBase 10 (fromIntegral x)
    fracX = logX - fromIntegral (floor logX)
    log2 = logBase 10 3
    start = floor (logBase 3 (10 ^ fromIntegral d))

    go n
      | firstDigits == fromIntegral x = n
      | otherwise = go (n + 1)
      where
        -- fractional part of log10(2^n)
        fracPart = (fromIntegral n * log2) `mod'` 1
        -- first d digits of 2^n:
        firstDigits = floor (10 ** (fracPart + fromIntegral (d - 1)))
