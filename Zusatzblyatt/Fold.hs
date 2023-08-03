foldlFak :: Int -> Int
foldlFak x = foldl (*) 1 [1..x]

foldrFak :: Int -> Int
foldrFak x = foldr (*) 1 [1..x]

foldlFib :: Int -> Int
foldlFib x = fst (foldl (\(x,y) _ -> (y,x+y)) (0,1) [1..x])

foldrFib :: Int -> Int
foldrFib x = fst (foldr (\_ (x,y) -> (y,x+y)) (0,1) [1..x])

foldlAnzahl :: [a] -> (a -> Bool) -> (Int, Int)
foldlAnzahl h@(x:xs) f = foldl (\(x,y) a -> if f a then (x+1,y) else (x,y+1)) (0,0) h

foldrAnzahl :: [a] -> (a -> Bool) -> (Int, Int)
foldrAnzahl h@(x:xs) f = foldr (\a (x,y) -> if f a then (x+1,y) else (x,y+1)) (0,0) h