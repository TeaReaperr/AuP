istLeer :: [a] -> Bool
istLeer [] = True
istLeer _ = False

snoc :: [a] -> a -> [a]
snoc [] a = [a]
snoc (x:xs) a = x : (snoc xs a)

laenge :: [a] -> Int
laenge [] = 0
laenge (x:xs) = succ (laenge xs)

erstesElement :: [a] -> a
erstesElement [] = error "Liste hat keine Element"
erstesElement (x:xs) = x

rest :: [a] -> [a]
rest [] = error "Liste ist leer"
rest (x:xs) = xs

letztesElement :: [a] -> a
letztesElement [] = error "Liste ist leer"
letztesElement (x:[]) = x
letztesElement (x:xs) = letztesElement xs

anfang :: [a] -> [a]
anfang [] = error "Liste ist leer"
anfang (x:[]) = []
anfang (x:xs) = x : anfang xs

nimm :: Int -> [a] -> [a]
nimm a [] = []
nimm 0 xs = []
nimm a (x:xs) = x : nimm (pred a) xs

verwerfe :: Int -> [a] -> [a]
verwerfe a [] = []
verwerfe 0 xs = xs
verwerfe a (x:xs) = verwerfe (pred a) xs

produkt :: (Num a) => [a] -> a
produkt [x] = x
produkt (x:xs) = x * produkt xs

verdopple :: (Num a) => [a] -> [a]
verdopple [] = []
verdopple (x:xs) = 2 * x : verdopple xs

verkette :: [a] -> [a] -> [a]
verkette (x:xs) (y:ys) = x : verkette xs (y:ys)

rueckwaerts :: [a] -> [a]
rueckwaerts [] = []
rueckwaerts (x:xs) = rueckwaerts xs ++ [x]

und :: [Bool] -> Bool
und [] = True
und (x:xs) = x && und xs
-- [a] = a:[]

nicht :: [Bool] -> [Bool]
nicht [] = []
nicht (x:xs) = (not x) : nicht xs

aufteilen :: Int -> [a] -> ([a],[a])
aufteilen _ [] = ([],[])
aufteilen x xs = (nimm x xs, verwerfe x xs)

verzahne :: [a] -> [b] -> [(a,b)]
verzahne (x:xs) (y:ys) = (x,y) : verzahne xs ys

aktualisiere :: [a] -> Int -> a -> [a]
aktualisiere (x:xs) 0 neu = [neu] ++ xs
aktualisiere (x:xs) index neu = [neu] ++ xs