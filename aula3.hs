import Data.Char

isVowel :: Char -> Bool
isVowel c = elem c "aeiouAEIOU"

addComma :: [String] -> [String]
addComma listString = map (++",") listString

htmlListItems :: [String] -> [String]
htmlListItems listString = map (\s -> "<LI>" ++ s ++ "</LI>") listString

semVogais :: String -> String
semVogais s = filter (\c -> not (isVowel c)) s

codifica :: String -> String
codifica s = map (\c -> if c /= ' ' then '-' else c) s

firstName :: String -> String
firstName s = takeWhile (\c -> c /= ' ') s

isInt :: String -> Bool
isInt s = length (takeWhile (\c -> notElem c "0123456789") s) == 0

lastName :: String -> String
lastName s = (last . words) s

userName :: String -> String 
userName s = [(toLower . head . firstName) s] ++ map toLower (lastName s)

encodeChar :: Char -> Char
encodeChar char
    | char == 'a' = '4'
    | char == 'e' = '3'
    | char == 'i' = '2'
    | char == 'o' = '1'
    | char == 'u' = '0'
    | otherwise = char

encodeName :: String -> String
encodeName s = map (encodeChar . toLower) s

betterEncodeChar :: Char -> String
betterEncodeChar char
    | char == 'a' = "4"
    | char == 'e' = "3"
    | char == 'i' = "1"
    | char == 'o' = "0"
    | char == 'u' = "00"
    | otherwise = [char]

betterEncodeName :: String -> String
betterEncodeName str = foldl (\s c -> s ++ (betterEncodeChar . toLower) c) [] str

func :: [String] -> [String]
func listStr = map (\str -> if length str > 10 then take 10 str else str ++ (replicate (10 - length str) '.')) listStr

pow2 :: Int
pow2 x = x^2