istElement :: (Ord a) => a -> [a] -> Bool
istElement _ [] = False
istElement a (b: xs)
    | a == b = True
    | a < b = istElement a xs
    | otherwise = False

istElement2 :: (Ord a) => a -> [a] -> Bool
istElement2 _ [] = False
istElement2 a (b: xs)
    | a == b = True
    | a < b = istElement2 a xs
    | otherwise = False

istTeilmenge :: (Ord a) => [a] -> [a] -> Bool
istTeilmenge [] _ = True
istTeilmenge _ [] = False
istTeilmenge (a:as) bs 
    | istElement a bs = istTeilmenge as bs
    | otherwise = False

istEchteTeilmenge :: (Num a, Ord a) => [a] -> [a] -> Bool
istEchteTeilmenge a b 
    | istTeilmenge a b && istTeilmenge b a = False
    | istTeilmenge a b = True
    | otherwise = False

vereinigung :: (Num a, Ord a) => [a] -> [a] -> [a]
vereinigung a [] = a
vereinigung [] b = b
vereinigung (a:as) (b:bs) 
    | a > b = a : vereinigung as (b : bs)
    | a == b = a : vereinigung as (bs)
    | otherwise = b : vereinigung (a:as) bs

schnitt :: (Num a, Ord a) => [a] -> [a] -> [a]
schnitt a [] = []
schnitt [] b = []
schnitt (a:as) (b:bs)
    | a > b = schnitt as (b:bs)
    | a == b = a : schnitt as (bs)
    | otherwise = schnitt (a:as) bs