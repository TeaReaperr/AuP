--If
schaltjahrIf :: Int -> Bool
schaltjahrIf x =
    if x `mod` 400 == 0 then True
    else if mod x 100 == 0 then False
    else 
        mod x 4 == 0

--Guards

schaltjahrGuards :: Int -> Bool
schaltjahrGuards x 
    | mod x 400 == 0 = True
    | mod x 100 == 0 = False
    | mod x 4 == 0 = True
    | otherwise = False

--Bool
schaltjahrBool :: Int -> Bool
schaltjahrBool x = 
    x `mod` 400 == 0 || x `mod` 4 == 0 && not (x `mod` 100 == 0)
        

   