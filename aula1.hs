main = do 
  putStrLn "Vamos testar as funcoes abaixo!"
  
-- Eleva um numero ao quadrado
-- Aqui temos um comentario!
square :: Int -> Int
square x = x^2

-- Verifica se um numero eh par (usa if/then/else para expressar funcao condicional)
-- A funcao 'mod' retorna resto da divisao inteira
isEven :: Int -> Bool
isEven n = if mod n 2 == 0 then True else False
-- Ou simplesmente:
-- isEven n = mod n 2 == 0

-- Gera um numero a partir de um caracter 
-- Note esta estrutura condicional em Haskell, usando'guardas' (|)
encodeMe :: Char -> Int
encodeMe c 
   | c == 'S'  = 0
   | c == 'N'  = 1
   | otherwise = undefined

-- Calcula o quadrado do primeiro elemento da lista
-- Note que '[Int]' designa uma lista de elementos do tipo Int 
squareFirst :: [Int] -> Int
squareFirst lis = (head lis)^2

-- Verifica se uma palavra tem mais de 10 caracteres
isLongWord :: String -> Bool -- isso é o mesmo que: isLongWord :: [Char] -> Bool
isLongWord s = length s > 10

-- Exercicios 3.1
sumSquares :: Int -> Int -> Int
sumSquares x y = x^2 + y^2

calcExprInt :: Int -> Int
calcExprInt n = 3*n^2 + 1

isNegative :: Int -> Bool
isNegative n = n < 0

calcExprFloat :: Float -> Float
calcExprFloat n = n^2 + n/2 + 1

addPrefix :: String -> String
addPrefix s = "Mr. " ++ s

addThisPrefix :: String -> String -> String
addThisPrefix a b = a ++ b

startsWithA :: String -> Bool
startsWithA s = head s == 'A'

isVerb :: String -> Bool
isVerb s = last s == 'r'

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

hasEqHeads :: [Int] -> [Int] -> Bool
hasEqHeads a b = head a == head b

isVowel2 :: Char -> Bool
isVowel2 c = elem c "aeiouAEIOU"

-- Exercicios 3.2
testAddPrefix :: [String] -> [String]
testAddPrefix s = map addPrefix s

onlyVowels :: String -> String
onlyVowels s = filter isVowel2 s

onlyNegatives :: [Int] -> [Int]
onlyNegatives i = filter isNegative i

calcExprFloats :: [Float] -> [Float]
calcExprFloats f = map calcExprFloat f

onlyLongWords :: [String] -> [String]
onlyLongWords s = filter isLongWord s

onlyEvens :: [Int] -> [Int]
onlyEvens i = filter isEven i

onlyVerbs :: [String] -> [String]
onlyVerbs s = filter isVerb s

isNumberValid :: Int -> Bool
isNumberValid n = n >= 1 && n <= 100

onlyValidNumbers :: [Int] -> [Int]
onlyValidNumbers i = filter isNumberValid i

funct :: String -> Int
funct s = length (filter (\a -> a == ' ') s)

isBornAfter :: Int -> Bool
isBornAfter n = (n - 2020 + 1980) > 0

onlyBornAfter :: [Int] -> [Int]
onlyBornAfter i = filter isBornAfter i

isEqual :: Char -> Char -> Bool
isEqual a b = a == b

charFound :: Char -> String -> Bool
charFound c s = length (filter (isEqual c) s) > 0
--charFound c s = length (filter ((\a b -> a == b) c) s) > 0