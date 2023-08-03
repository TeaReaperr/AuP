module AvlTree (
    Menge,
    leer,
    einfuegen,
    loeschen,
    vereinigung,
    --schnitt,
    --differenz,
    istLeer,
    istElement,
    istTeilmenge,
    istEchteTeilmenge,
    minimalesElement,
    maximalesElement
) where 
    import Avltree
    
    type Menge el = AVLBaum el
    
    leer :: Menge el
    leer = AVLBlatt

    einfuegen :: (Ord el) => el -> Menge el -> Menge el
    einfuegen = avlEinfuegen

    loeschen :: (Ord el) => el -> Menge el -> Menge el
    loeschen = avlLoeschen 

    vereinigung :: (Num el, Ord el) => Menge el -> Menge el -> Menge el
    vereinigung a AVLBlatt = a
    vereinigung AVLBlatt b = b
    vereinigung h@(AVLKnoten _ lL vL rL) k@(AVLKnoten _ lR vR rR) = vereinigung (einfuegen vR h) (loeschen vR k)

{-
    schnitt :: (Num el, Ord el) => Menge el -> Menge el -> Menge el
    schnitt a AVLBlatt = AVLBlatt
    schnitt AVLBlatt b = AVLBlatt
    schnitt h@(AVLKnoten _ lL vL rL) k@(AVLKnoten _ lR vR rR)
        | vL == vR = schnitt(einfuegen vR h) (loeschen vR k)
        | otherwise = 

    differenz :: Ord el => Menge el -> Menge el -> Menge el
-}
    istLeer :: Ord el => Menge el -> Bool
    istLeer AVLBlatt = True
    istLeer AVLKnoten {} = False

    istElement :: (Ord el) => el -> Menge el -> Bool
    istElement _ AVLBlatt = False
    istElement el (AVLKnoten _ l v r)
        | el == v = True
        | istElement el l = True
        | istElement el r = True
        | otherwise = False


    istTeilmenge :: (Ord el) => Menge el -> Menge el -> Bool
    istTeilmenge AVLBlatt _ = True
    istTeilmenge _ AVLBlatt = False
    istTeilmenge h@(AVLKnoten _ lL vL rL) k@(AVLKnoten _ lR vR rR) = istElement vL k && istTeilmenge lL k && istTeilmenge lR k

    istEchteTeilmenge :: (Num el, Ord el) => Menge el -> Menge el -> Bool
    istEchteTeilmenge h@(AVLKnoten _ lL vL rL) k@(AVLKnoten _ lR vR rR)
        | istTeilmenge h k && istTeilmenge k h = False
        | istTeilmenge h k = True
        | otherwise = False

    minimalesElement :: Menge el -> el
    minimalesElement (AVLKnoten _ AVLBlatt v r) = v
    minimalesElement (AVLKnoten _ l v r) = minimalesElement l

    maximalesElement :: Menge el -> el
    maximalesElement (AVLKnoten _ l v AVLBlatt) = v
    maximalesElement (AVLKnoten _ l v r) = maximalesElement r