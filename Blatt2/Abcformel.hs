-- a : Double
-- b : Double
-- c : Double
--abcLet
abcformelLet :: Double -> Double -> Double -> (Double, Double)
abcformelLet a b c =
    let
        d = sqrt(b*b -4 * a * c)
        e = 2 * a
        x1 = ((-b) + sqrt(b**2 - 4*a*c)) / (2*a)
        xf = ((-b) - sqrt(b**2 - 4*a*c)) / (2*a)
    in
        ((-b + d) / e, (-b -d) / e )

--abcWhere
abcformelWhere :: Double -> Double -> Double -> (Double, Double)
abcformelWhere a b c =
    (x1,xf)
        where
            x1 = ((-b) + sqrt(b**2 - 4*a*c)) / (2*a)
            xf = ((-b) - sqrt(b**2 - 4*a*c)) / (2*a)
