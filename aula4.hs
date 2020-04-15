import Data.List

anoIdade :: Int -> (Int, Int)
anoIdade i = (i, 2020 - i)

selectName :: (String, Int, Int) -> String
selectName (str, _, _) = str

allNames :: [(String, Int, Int)] -> [String]
allNames lisTupla = map selectName lisTupla

allNames2 :: [(String, Int, Int)] -> [String]
allNames2 lisTupla = map (\(str, _, _) -> str) lisTupla

distance :: (Float, Float) -> (Float, Float) -> Float
distance p0 p1 = sqrt ((fst p1 - fst p0)^2 + (snd p1 - snd p0)^2)

funcSite :: [(String, String)] -> [String]
funcSite lisTuple = map (\tuple -> "<a href=\"" ++ snd tuple ++ "\">" ++ fst tuple ++ "</a>") lisTuple

calculaArea :: (Float, Float, Float, Float) -> Float
calculaArea (_, _, w, h) = w * h

calculaAreas :: [(Float, Float, Float, Float)] -> [Float]
calculaAreas lisRect = map calculaArea lisRect

calculaAreas2 :: [(Float, Float, Float, Float)] -> [Float]
calculaAreas2 lisRect = map (\(_, _, w, h) -> w * h) lisRect

testSort1 :: [Int] -> [Int]
testSort1 lis = sort lis

testSort2 :: [String] -> [String]
testSort2 lis = sort lis

testZip :: [Int] -> [Int] -> [(Int, Int)]
testZip lisA lisB = filter (\tuple -> fst tuple + snd tuple < 10) (zip lisA lisB)