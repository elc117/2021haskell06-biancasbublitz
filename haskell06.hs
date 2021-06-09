-- Prática 06 de Haskell
-- Nome: Bianca Sabrina Bublitz

-- 01
ends :: [Int] -> [Int]
ends x = (head x) : (last x) : []

-- 02
deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame xs

-- 03
deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) = if x > 2
  then x : deduzame2 xs
  else deduzame2 xs

-- 04
geraTabela :: Int -> [(Int,Int)]
geraTabela 0 = []
geraTabela n = (n, n^2) : geraTabela (n-1)

-- 05
contido :: Char -> String ->  Bool
contido a "" = False
contido a (x:xs) = if x == a
  then True
  else contido a xs

-- 06
translate :: [(Float, Float)] -> [(Float, Float)]
translate [] = []
translate (x:xs) = ((fst x + 2), (snd x + 2)) : translate xs

-- 07
countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs) = if length x > 5
  then 1 + countLongs xs
  else countLongs xs

--08
onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs) = if length x > 5
  then x : onlyLongs xs
  else onlyLongs xs