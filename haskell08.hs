import Data.Char

isBin :: String -> Bool
isBin [] = False
isBin (x:xs) = (if x == '0' || x == '1' then True else False) && (if length xs > 0 then isBin xs else True)

isBin' :: String -> Bool
isBin' [] = False
isBin' str = length (filter (\x -> x /= '0' && x /= '1') str) == 0

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec (x:xs) exp = x * 2^exp + (if length xs > 0 then auxBin2Dec xs (exp - 1) else 0)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec' :: [Int] -> Int
bin2dec' [] = undefined
bin2dec' bits = foldl1 (\n bit -> n * 2 + bit) bits

dec2bin :: Int -> [Int]
dec2bin n = if n > 1 then dec2bin (div n 2) ++ [mod n 2] else [n]

isHex :: String -> Bool
isHex [] = False
isHex str = length (filter (\c -> notElem (toUpper c) "0123456789ABCDEF") str) == 0

hex2dec :: String -> Int
hex2dec [] = undefined
hex2dec str = foldl (\n hex -> n * 16 + digitToInt hex) 0 str

dec2hex :: Int -> String
dec2hex n = if n > 15 then dec2hex (div n 16) ++ [intToDigit (mod n 16)] else [intToDigit n]
