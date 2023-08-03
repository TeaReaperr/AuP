{-
1.
Beispielsweise wird das letzte Element der Liste wird als Pivotelement bestimmt.
Von vorn wird nach Element größer gesucht und von hinten ein kleineres.
Die ersten von diesen, welche gefunden wurden werden vertauscht.
So lang weiter durchführen, bis die Suche aufeinandertrifft, also beim Gleichen Element angekommen ist.
Vorn stehen jetzt alle Elemente > Pivot und hinten alle < Pivot.
Pivotelement wird jetzt an den Index dazwischen geschoben und bleibt für immer dort.
Die vordere Teilliste wird jetzt wieder genau so abgearbeitet und danach die Hintere auch.
-}
equal :: Ord a => a -> [a] -> [a]
equal a [] = []
equal a (x:xs) =
    if x == a then x : equal a xs else equal a xs

smaller :: Ord a => a -> [a] -> [a]
smaller a [] = []
smaller a (x:xs) =
    if x < a then x : smaller a xs else smaller a xs

larger :: Ord a => a -> [a] -> [a]
larger a [] = []
larger a (x:xs) =
    if x > a then x : larger a xs else larger a xs

pivotM :: Ord a => a -> a -> a ->  a
pivotM x y z 
    | x < y && y < z = y 
    | x < z && z < y = z
    | y < x && x < z = x
    | y < z && z < x = z
    | z < x && x < y = x
    | otherwise = y

quickSortFirst :: Ord a => [a] -> [a]
quickSortFirst [] = []
quickSortFirst [a] = [a]
quickSortFirst h@(x:xs) = quickSortFirst(smaller x h) ++ [x] ++ quickSortFirst(larger x h)

quickSortMiddle :: Ord  a => [a] -> [a]
quickSortMiddle [] = []
quickSortMiddle [a] = [a]
quickSortMiddle (a:[b]) = if a < b then a:[b] else b:[a]
quickSortMiddle h@(x:y:z:xs) =
    let
        d = pivotM x y z  
    in
        quickSortMiddle(smaller d h) ++ [d] ++ quickSortMiddle(larger d h)