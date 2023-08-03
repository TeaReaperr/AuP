module Menge (
    Menge,
    leer,
    einfuegen,
    loeschen,
    vereinigung,
    schnitt,
    differenz,
    istLeer,
    istElement,
    istTeilmenge,
    istEchteTeilmenge,
    minimalesElement,
    maximalesElement
) where
    data Menge el = Menge [el] deriving (Eq)
    instance (Show el) => Show (Menge el) where
        show (Menge liste) = "{" ++ init (tail (show liste)) ++ "}"

    leer :: Menge el
    leer = Menge []

    einfuegen :: (Ord el) => el -> Menge el -> Menge el
    einfuegen wert (Menge []) = Menge [wert]
    einfuegen wert (Menge h@(x:xs))
        | wert == x = Menge h
        | wert < x = 
            let (Menge liste) = einfuegen wert (Menge xs)
            in Menge (x: liste)
        | otherwise = Menge (wert:x:xs)

    loeschen :: (Ord el) => el -> Menge el -> Menge el
    loeschen _ (Menge []) = Menge []
    loeschen wert (Menge (x:xs)) 
        | x == wert = Menge xs
        | otherwise = einfuegen x (loeschen wert (Menge xs))
    
    vereinigung :: (Num el, Ord el) => Menge el -> Menge el -> Menge el
    vereinigung a (Menge[]) = a
    vereinigung (Menge[]) b = b
    vereinigung (Menge(a:as)) (Menge(b:bs)) 
        | a > b = einfuegen a (vereinigung (Menge as) (Menge(b:bs)))
        | a == b = einfuegen a (vereinigung (Menge as) (Menge bs))
        | otherwise = einfuegen b  (vereinigung (Menge (a:as)) (Menge bs))

    schnitt :: (Num el, Ord el) => Menge el -> Menge el -> Menge el
    schnitt a (Menge[]) = Menge[]
    schnitt (Menge[]) b = Menge[]
    schnitt (Menge(a:as)) (Menge(b:bs))
        | a > b = schnitt (Menge as) (Menge(b:bs))
        | a == b = einfuegen a (schnitt (Menge as) (Menge bs))
        | otherwise = schnitt (Menge(a:as)) (Menge bs)

    differenz :: Ord el => Menge el -> Menge el -> Menge el
    differenz (Menge []) _ = Menge []
    differenz (Menge (x:xs)) (Menge []) = Menge xs
    differenz (Menge (x:xs)) (Menge (y:ys))
        | x > y = differenz (Menge(x:xs)) (Menge ys)
        | x < y = einfuegen x (differenz (Menge xs) (Menge (y:ys)))
        | otherwise = differenz (Menge xs) (Menge ys)

    istLeer :: Ord el => Menge el -> Bool
    istLeer (Menge []) = True
    istLeer (Menge _) = False

    istElement :: (Ord el) => el -> Menge el -> Bool
    istElement _ (Menge[]) = False
    istElement a (Menge(b: xs))
        | a == b = True
        | a < b = istElement a (Menge xs)
        | otherwise = False
    
    istTeilmenge :: (Ord el) => Menge el -> Menge el -> Bool
    istTeilmenge (Menge[]) _ = True
    istTeilmenge _ (Menge[]) = False
    istTeilmenge (Menge(a:as)) (Menge bs) 
        | istElement a (Menge bs) = istTeilmenge (Menge as) (Menge bs)
        | otherwise = False

    istEchteTeilmenge :: (Num el, Ord el) => Menge el -> Menge el -> Bool
    istEchteTeilmenge (Menge a) (Menge b) 
        | istTeilmenge (Menge a) (Menge b) && istTeilmenge (Menge b) (Menge a) = False
        | istTeilmenge (Menge a) (Menge b) = True
        | otherwise = False

    minimalesElement :: Menge el -> el
    minimalesElement (Menge []) = error "Leere Menge"
    minimalesElement (Menge h@(x:xs)) = last h

    maximalesElement :: Menge el -> el
    maximalesElement (Menge []) = error "Leere Menge"
    maximalesElement (Menge h@(x:xs)) = x