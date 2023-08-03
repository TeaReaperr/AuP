--Robertson
robertson :: Int -> Int -> Int -> Int
robertson t m j =
    let
        a = m + 10 
        b = ((m-14) `quot` 12) + j
        c = a - 12 * (a `quot` 13)
        d = ((13*c - 1) `quot` 5)
        e = ((5*(b `mod` 100)) `quot` 4)
    in
        (d + t + 77 + e + (b `quot` 400) - 2 * (b `quot` 100)) `mod` 7


--Wochentag
wochentag :: Int -> String
wochentag x
    | x == 0 = "Sonntag"
    | x == 1 = "Montag"
    | x == 2 = "Dienstag"
    | x == 3 = "Mittwoch"
    | x == 4 = "Donnerstag"
    | x == 5 = "Freitag"
    | x == 6 = "Samstag"
    | x < 0 || x > 6 = "Falsche Eingabe"