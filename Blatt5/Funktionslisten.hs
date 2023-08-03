resultat :: (Ord a) => [(a,b)] -> a -> b
resultat ((x,fx):xs) a
    | x == a = fx
    | x < a = resultat xs a
resultat _ _ = error "Nicht gefunden"

evtlResultat :: (Ord a) => [(a,b)] -> a -> Maybe b
evtlResultat [] _ = Nothing
evtlResultat ((x,fx):xs) a
    | x == a = Just fx
    | x > a = Nothing
    | otherwise = evtlResultat xs a