import Data.Char

func1 :: [Int] -> Bool
func1 lst = expr1 > expr2
 where expr1 = head lst + 20
       expr2 = last lst * 2

func2 :: String -> Bool
func2 str =
 let digits = map digitToInt str
     sumDigits = sum digits
  in sumDigits > 50

func4 :: IO()
func4 = do
 str <- getLine
 putStrLn ("Nome: " ++ str)

isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      expr1 = (sum $ zipWith (*) digitos1 [10,9..2]) `mod` 11
      dv1 = if expr1 < 2 then 0 else 11-expr1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      expr2 = (sum $ zipWith (*) digitos2 [11,10..2]) `mod` 11
      dv2 = if expr2 < 2 then 0 else 11-expr2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

main = do
  cpf <- getLine
  let digitos = (map digitToInt cpf)
      result = isCpfOk digitos
  putStrLn (show result)