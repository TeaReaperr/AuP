oder :: Bool -> Bool -> Bool
oder = (||)
nicht :: Bool -> Bool
nicht = not
und :: Bool -> Bool -> Bool
und a b = nicht(nicht a || nicht b)
--und = (&&)
darausFolgt :: Bool -> Bool -> Bool
darausFolgt a b = (a `und` b) `oder` nicht a
genauDannWenn :: Bool -> Bool -> Bool
genauDannWenn a b = (a `und` b) `oder` (nicht a `und` nicht b) 
entwederOder :: Bool -> Bool -> Bool
entwederOder a b = (nicht a `und` b) `oder` (a `und` nicht b)