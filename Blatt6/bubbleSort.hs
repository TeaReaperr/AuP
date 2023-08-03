bubbleSort :: Ord el => [el] -> [el]
bubbleSort [] = []
bubbleSort [el] = [el]
bubbleSort xs = 
    let ys = bubble xs
    in bubbleSort(init ys) ++ [last ys]
{-
[] trivial
[el] trivial
T(xs) = n
T((init ys) = n-1
n-1 < n
(N, <=)
Terminiert
-}

bubble :: (Ord el) => [el] -> [el]
bubble [] = []
bubble [a] = [a]
bubble (x:(y:ys)) = if x < y then x : bubble (y:ys) else y : bubble (x:ys)
--Fall Zeile 2./3.: Trivial
--n = 1 oder 0
--Fall Zeile 11:
--n = Länge der Liste
--T(n-1)
--n-1 < n
--(N, <): reflexiv, antisymmetrisch, transitiv  