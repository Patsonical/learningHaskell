cHex :: Char -> Int
cHex x
 | ord >= 48 && ord <= 57       = ord - 48
 | ord >= 65 && ord <= 70       = ord - 55
 | ord >= 97 && ord <= 102      = ord - 87
 | otherwise                    = 0
        where ord = fromEnum x

strip :: String -> String
strip [] = []
strip (x:xs) =
        if x `elem` "0123456789AaBbCcDdEeFf" then x:(strip xs)
        else strip xs

fromHex :: String -> Int
fromHex [] = 0
fromHex y  = cHex (head x) + fromHexm 16 (tail x)
        where x = reverse $ strip y

fromHexm :: Int -> String -> Int
fromHexm m [] = 0
fromHexm m (x:xs) = (m * (cHex x)) + (fromHexm (16*m) xs)
