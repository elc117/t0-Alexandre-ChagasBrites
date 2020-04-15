add10toall :: [Int] -> [Int]
add10toall lis = [x + 10 | x <- lis]

multN :: Int -> [Int] -> [Int]
multN n lis = [x * n | x <- lis]

applyExpr :: [Int] -> [Int]
applyExpr lis = [x * 3 + 2 | x <- lis]

addSuffix :: String -> [String] -> [String]
addSuffix str lis = [x ++ str | x <- lis]

selectgt5 :: [Int] -> [Int]
selectgt5 lis = [x | x <- lis, x > 5]

sumOdds :: [Int] -> Int
sumOdds lis = sum [x | x <- lis, odd(x)]

selectExpr :: [Int] -> [Int]
selectExpr lis = [x | x <- lis, x >= 20, x <= 50]

countShorts :: [String] -> Int
countShorts lis = length [x | x <- lis, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr lis = [x | x <- [x^2/2 | x <- lis], x > 10]

trSpaces :: String -> String
trSpaces str = [if x == ' ' then '-' else x | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd lis = [snd x | x <- lis]

dotProd :: [Int] -> [Int] -> Int
dotProd lisA lisB = sum [fst x * snd x | x <- zip lisA lisB]

genRects :: Int -> (Int, Int) -> [(Float, Float, Float, Float)]
genRects n (x, y) = [(fromIntegral(x) + 5.5 * fromIntegral(i), fromIntegral(y), 5.5, 5.5) | i <- [0..(n - 1)]]