ends :: [Int] -> [Int]
ends [x] = [x]
ends (x:xs) = x : [last xs]

deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = 2 * x : deduzame xs

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 lst = if (head lst) > 2
  then head lst : deduzame2 (tail lst) 
  else deduzame2 (tail lst)

geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n, n^2) : geraTabela (n-1)

contido :: Char -> String -> Bool
contido _ [] = False
contido c (x:xs) = c == x || contido c xs

translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate ((a, b):xs) = (a + 2, b + 2) : translate xs

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = (if length x > 5 then 1 else 0) + countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5 then x : onlyLongs xs else onlyLongs xs
