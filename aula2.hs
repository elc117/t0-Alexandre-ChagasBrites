main = do 
  putStrLn "Vamos testar as funcoes abaixo!"
  
square :: Int -> Int
square x = x^2

squareAll :: [Int] -> [Int]
squareAll lis = map square lis 

ficaemcasa :: String -> String
ficaemcasa fulano = fulano ++ ", fica em casa!"

quarentena :: [String] -> [String]
quarentena pessoas = map ficaemcasa pessoas

podesair :: String -> Bool
podesair profissao = profissao == "Medico"

idadeadulta :: Int -> Bool
idadeadulta idade = idade >= 18

--Exercícios
--3.1
temFebre :: Float -> Bool
temFebre n = n > 37.8

verificaFebre :: [Float] -> [Float]
verificaFebre listN = filter temFebre listN

--3.2
verificaFebre2 :: [Float] -> [Float]
verificaFebre2 listN = filter (\n -> n > 37.8) listN

--3.3
--map (\m -> n + 2) [1,2,3]

--3.4 map (\n -> n^2 + 2) [1,2,3]
-- [3, 7, 11]

--3.5
func :: [Int] -> [Int]
func listA = map (\a -> a^2 + a + 2) listA