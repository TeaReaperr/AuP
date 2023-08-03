module Avltree(
    AVLBaum(..),
    hoehe,
    balance,
    avlKnoten,
    verbinden,
    value,
    avlEinfuegen,
    avlLoeschen
)where

    data AVLBaum el = AVLBlatt
        | AVLKnoten Int (AVLBaum el) el (AVLBaum el) deriving Show

    hoehe :: AVLBaum el -> Int
    hoehe AVLBlatt = 0
    hoehe (AVLKnoten x l v r) = x

    balance :: AVLBaum el -> AVLBaum el -> Int
    balance l r = hoehe l - hoehe r

    avlKnoten :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
    avlKnoten l v r = AVLKnoten (1 + max (hoehe l) (hoehe r)) l v r

    verbinden :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
    verbinden l v r = 
        if abs (hoehe l - hoehe r) > 1 
        then if hoehe r > hoehe l
            then rotiereR l v r
            else rotiereL l v r
        else avlKnoten l v r

    links :: AVLBaum el -> AVLBaum el
    links (AVLKnoten h links v rechts) = links

    rechts :: AVLBaum el -> AVLBaum el
    rechts (AVLKnoten h links v rechts) = rechts

    value :: AVLBaum el -> el
    value (AVLKnoten h links v rechts) = v

    rotiereR :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
    rotiereR l v (AVLKnoten _ linksR vR rechtsR) = 
        if hoehe rechtsR > hoehe linksR
        then avlKnoten (avlKnoten l v linksR) vR rechtsR
        else avlKnoten (avlKnoten l v (links linksR)) (value linksR) (avlKnoten (rechts linksR) vR rechtsR)

    rotiereL :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
    rotiereL (AVLKnoten _ linksL vL rechtsL) v r =
        if hoehe rechtsL < hoehe linksL 
        then avlKnoten linksL vL (avlKnoten rechtsL v r)
        else avlKnoten (avlKnoten linksL vL (links rechtsL)) (value rechtsL) (avlKnoten (rechts rechtsL) v r)

    avlEinfuegen :: (Ord el) => el -> AVLBaum el -> AVLBaum el
    avlEinfuegen el AVLBlatt = avlKnoten AVLBlatt el AVLBlatt
    avlEinfuegen el h@(AVLKnoten _ l n r) 
        | el == n = h 
        | el < n = verbinden (avlEinfuegen el l) n r
        | otherwise = verbinden l n (avlEinfuegen el r)

    avlLoeschen :: Ord el => el -> AVLBaum el -> AVLBaum el
    avlLoeschen el AVLBlatt = AVLBlatt
    avlLoeschen el h@(AVLKnoten _ AVLBlatt v AVLBlatt)
        | el == v = AVLBlatt
        | otherwise = h
    
    avlLoeschen el h@(AVLKnoten _ l v AVLBlatt) 
        | el == v = l
        | el < v = avlKnoten l v AVLBlatt
        | otherwise = h

    avlLoeschen el t@(AVLKnoten _ l v r)
        | el == v = 
            let x = minimalesElement r
                r' = avlLoeschen x r
            in verbinden l x r'
        | el < v = avlKnoten l v r
        | otherwise = avlKnoten l v (avlLoeschen el r)
    
    minimalesElement :: AVLBaum el -> el
    minimalesElement (AVLKnoten _ AVLBlatt v r) = v
    minimalesElement (AVLKnoten _ l v r) = minimalesElement l