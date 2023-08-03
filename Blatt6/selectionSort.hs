selectionSort :: Ord el => [el] -> [el]
selectionSort [] = []
selectionSort h@(x:xs) = Main.minimum h : selectionSort (loesche (Main.minimum h) h)
{-
[] trivial
T(x:xs) = n
T(x) = länge der Liste
T(loesche (minimum h) h) = n - 1
n-1 < n
(N, <=)
Terminiert
-}

minimum :: Ord el => [el] -> el
minimum [] = error "Liste ist leer" 
minimum [x] = x
minimum (x:xs) = if x < Main.minimum xs then x else Main.minimum xs
{-
[] trivial
[x] trivial
T(x:xs) = n
T(x) = länge der Liste x
T(xs) = n-1
n-1 < n
(N, <=)
Terminiert
-}

loesche :: Ord el => el -> [el] -> [el]
loesche _ [] = []
loesche y (x:xs) = if y == x then xs else x : loesche y xs
{-
_ [] trivial
T(x:xs) = n
T(x) = länge der Liste x
T(xs) = n-1
n-1 < n
(N, <=)
Terminiert
-}