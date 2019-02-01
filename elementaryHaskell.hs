-- More Functions --

doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs

multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (x:xs) = (m * x) : multiplyList m xs
