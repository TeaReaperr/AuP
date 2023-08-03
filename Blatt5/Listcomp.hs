--hab dir mal alles rauskommentiert, was nicht Kompiliert

kreuzprodukt :: [a] -> [b]-> [(a,b)]
kreuzprodukt x y = [(a,b) | a <- x, b<-y]

geordnentePaare :: [Integer] -> [(Integer,Integer)]
geordnentePaare liste = [(a,b) | a <- liste, b<-liste, a<=b]

zusammenhaengen :: [[a]] -> [a]
zusammenhaengen liste = [x | y <- liste, x <- y]

--for y in liste:
--    for x in y:
--        [x]

zusammenhaengen2 :: [[a]] -> [a]
zusammenhaengen2 [] = []
zusammenhaengen2 (x:xs) = x ++ (zusammenhaengen2 xs)

--zweite :: [(a,b)] -> [b]

--zweite2 :: [(a,b)] -> [b]

durchFuenfTeilbar :: [Integer] -> [Integer]
durchFuenfTeilbar n = [x | x <- n, mod x 5 == 0]

--[1,2,3,4]

--durchFuenfTeilbar2 :: [Integer] -> [Integer]
--durchFuenfTeilbar2 [] = []
--durchFuenfTeilbar2 (x:xs) = if mod x 5 == 0 then  else 

teiler :: Integer -> [Integer]
teiler n = [x | x <- [1..n], mod n x == 0]

helper :: Integer -> Integer -> [Integer]
helper n 1 = [1]
helper n x = if mod n x == 0 then x: helper n (x-1) else helper n (x-1)

teiler2 :: Integer -> [Integer]
teiler2 n = helper n n

--istPrim :: Integer -> Bool
--istPrim n =
--    if (teiler n) not 0 then False else True

--primzahlen :: Integer -> [Integer]
--primzahlen n = [x | x <- [1..n], mod n x not 0]

--primzahlen2 :: Integer -> [Integer]
--primzahlen2 n = if mod n x not 0 then x : primzahlen2 xs else x xs

paare :: [a] -> [(a,a)]
paare [] = []
paare [a] = []
paare (x:y:xs) = (x,y) : paare (y:xs)

--sortiert :: Ord a => [a] -> Bool

--sortiert2 :: Ord a => [a] -> Bool

--anzKleinBuch :: String -> Int
--anzKleinBuch s = [x | x <- s, isLower]

--anzKleinBuch2 :: String -> Int

--anzahl :: Char -> String -> Int

--anzahl2 :: Char -> String -> Int

--wiederhole :: Integer -> a -> [a]

wiederhole2 :: Integer -> a -> [a]
wiederhole2 n a = if n >= 1 then a : wiederhole2 (n-1) a else []

--istPerfekt :: Integer -> Bool

--perfektezahlen :: Integer -> Integer

--positionen :: Eq a => a -> a -> [a] -> [Int]

--positionen2 :: Eq a => a -> a -> [a] -> [Int]