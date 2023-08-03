insertAt :: a -> Int -> [a] -> [a]
insertAt x _ [] = [x]
insertAt x 0 h@(y:ys) = x:h
insertAt x i (y:ys) = y : insertAt x (i - 1) ys

set :: Ord a => [a] -> a -> Int -> [a]
set [] _ _ = []
set h@(x:xs) y i = take i h ++ [y] ++ drop (i+1) h

swap :: Ord a => Int -> Int -> [a] -> [a]
swap _ _ [] = []
swap i j h@(x:xs) = 
    let x = h !! i
        y = h !! j
    in set (set h x j) y i  

swapAt :: Ord a => [a] -> [a]
swapAt [] = []
swapAt h@(x:xs) = 
    if x > last h
    then swap 0 (length h - 1) h
    else h

stoogeSort :: Ord a => [a] -> [a]
stoogeSort [] = []
stoogeSort [x] = [x]
stoogeSort [x, y]
    | x > y = [y, x]
    | otherwise = [x, y]

stoogeSort h@(x:xs) =
    let getauscht = swapAt h
        erste = (stoogeSort ((getThird 1 getauscht) ++ getThird 2 getauscht)) ++ getThird 3 getauscht
        letzte = (getThird 1 erste) ++ (stoogeSort ((getThird 2 erste) ++ getThird 3 erste))
    in (stoogeSort ((getThird 1 letzte) ++ getThird 2 letzte)) ++ getThird 3 letzte

getThird :: Int -> [a] -> [a]
getThird i h@(x:xs) = 
    let l = length h
        m = div l 3
    in    
        if i == 1 
        then take m h
        else if i == 2
            then drop m (take (2*m) h)
            else drop (2 * m) h

{-
Stoogesort überprüft die Größe des ersten und des letzten Elementes.
Wenn das erste Element größer als das letzte ist werden beide vertauscht.
Dieses Verfahren wird nur mit den ersten zwei Dritteln der Liste durchgeführt, dann mit den
letzten zwei Dritteln und dann wieder mit den ersten beiden Dritteln, bis die Liste sortiert ist. 
-}