andWithFoldr :: [Bool] -> Bool
andWithFoldr h@(y:ys) = foldr (&&) True h

andWithFoldl :: [Bool] -> Bool
andWithFoldl h@(y:ys) = foldl (&&) True h

orWithFoldr :: [Bool] -> Bool
orWithFoldr h@(x:xs)= foldr (||) False h

orWithFoldl :: [Bool] -> Bool
orWithFoldl h@(x:xs)= foldl (||) False h

sumWithFoldr :: (Num a) => [a] -> a
sumWithFoldr [] = 0
sumWithFoldr (x:xs) = foldr (+) x xs

sumWithFoldl :: (Num a) => [a] -> a
sumWithFoldl [] = 0
sumWithFoldl (x:xs) = foldl (+) x xs

productWithFoldr :: (Num a) => [a] -> a
productWithFoldr [] = 0
productWithFoldr (x:xs) = foldl (*) x xs

productWithFoldl :: (Num a) => [a] -> a
productWithFoldl [] = 0
productWithFoldl (x:xs) = foldl (*) x xs

concatWithFoldr :: [[a]] -> [a]
concatWithFoldr h@((x:xs)) = foldr (++) [] h

concatWithFoldl :: [[a]] -> [a]
concatWithFoldl h@((x:xs)) = foldl (++) [] h

reverseWithFoldr :: [a] -> [a]
reverseWithFoldr = foldr (\x xs -> xs ++ [x]) []

reverseWithFoldl :: [a] -> [a]
reverseWithFoldl = foldl (flip (:)) []

mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f = foldr (\x xs -> f x : xs) []

mapWithFoldl :: (a -> b) -> [a] -> [b]
mapWithFoldl f = foldl (\x xs -> x ++ [f xs]) []

anyWithFoldr :: (a -> Bool) -> [a] -> Bool
anyWithFoldr f h@(x:xs)= foldr ((||) . f) False h

anyWithFoldl :: (a -> Bool) -> [a] -> Bool
anyWithFoldl f h@(x:xs)= foldl (\x xs -> x || f xs) False h

allWithFoldr :: (a -> Bool) -> [a] -> Bool
allWithFoldr f h@(x:xs) = foldr ((&&) . f) True h

allWithFoldl :: (a -> Bool) -> [a] -> Bool
allWithFoldl f h@(x:xs) = foldl (\x xs -> x && f xs) True h

filterWithFoldr :: (a -> Bool) -> [a] -> [a]
filterWithFoldr f = foldr (\x xs -> if f x then x : xs else xs) []

filterWithFoldl :: (a -> Bool) -> [a] -> [a]
filterWithFoldl f = foldl (\x xs -> if f xs then xs : x else x) []

maximumWithFoldr1 :: (Ord a) => [a] -> a
maximumWithFoldr1 = foldr1 max

maximumWithFoldl1 :: (Ord a) => [a] -> a
maximumWithFoldl1 = foldl1 max

minimumWithFoldr1 :: (Ord a) => [a] -> a
minimumWithFoldr1 = foldr1 min

minimumWithFoldl1 :: (Ord a) => [a] -> a
minimumWithFoldl1 = foldl1 min

appendWithFoldr :: [a] -> [a] -> [a]
appendWithFoldr xs ys = foldr (:) ys xs

appendWithFoldl :: [a] -> [a] -> [a]
appendWithFoldl xs ys = foldl (flip (:)) ys (reverseWithFoldl xs)

lengthWithFoldr :: [a] -> Int
lengthWithFoldr = foldr (\_ acc -> acc + 1) 0

lengthWithFoldl :: [a] -> Int
lengthWithFoldl = foldl (\acc _ -> acc + 1) 0

takeWithFoldl :: Int -> [a] -> [a]
takeWithFoldl n = foldl (\xs x -> if lengthWithFoldl xs < n then xs ++ [x] else xs) []

hintereinanderAnwendenFoldr :: [wert -> wert] -> wert -> wert
hintereinanderAnwendenFoldr fs a = foldr (\x a -> x a) a (reverse fs)

hintereinanderAnwendenFoldl :: [wert -> wert] -> wert -> wert
hintereinanderAnwendenFoldl fs a = foldl (\a x -> x a) a fs