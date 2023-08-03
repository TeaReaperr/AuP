slowSort :: Ord a => [a] -> [a]
slowSort [] = []
slowSort (x:xs) = 
    let
        klein = filter (< x) xs
        groß = filter (>= x) xs
    in
        slowSort klein ++ [x] ++ slowSort groß

{-
SlowSort funktioniert, indem die erste Hälfte und die zweite Hälfte jeweils mit Bubblesort sortiert
und dann zusammengefügt werden. Das größte Element wird hierbei an das Ende der liste getan. 
Danach nimmt man sich wieder zwei Teillisten ohne die sortierten Elemente und fängt von vorn an, 
bis die Liste sortiert ist.
-}