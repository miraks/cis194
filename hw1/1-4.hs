toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = map (\c -> read [c]) $ show n
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther =
  doubleEven . addIndexes
  where
    addIndexes = flip zip [1..]
    doubleEven = map (\(d, i) -> if even i then 2*d else d)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate num =
  sum num `mod` 10 == 0
  where
    sum = sumDigits . doubleEveryOther . toDigitsRev
