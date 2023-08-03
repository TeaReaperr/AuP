--plus
plus :: Int -> Int -> Int
plus a 0 = a
plus a b = succ ( plus a ( pred b ) )

--mal
mal :: Int -> Int -> Int
mal a 0 = 0
mal a 1 = a
mal a b = plus b (mal b (pred a)) --4*4 4+3*4 4+4+2*4 4+4+4+1*4 4+4+4+4

--potenz
potenz :: Int -> Int -> Int
potenz a 0 = 1
potenz a 1 = a
potenz a b = mal a (potenz a (pred b)) --2^5 2*2^4 2*2*2^3 2*2*2*2^2 2*2*2*2*2