module Vektor(
    Vektor,
    einsVektor,
    vektorAddition,
    vektorMultiplikation, 
    vektorSkalarprodukt
)where   

    data Vektor v = Vektor {x :: v, y :: v, z :: v} deriving Show
    einsVektor :: (Num t) => Vektor t
    einsVektor = Vektor 1 1 1

    vektorAddition :: (Num t) => Vektor t -> Vektor t -> Vektor t
    vektorAddition (Vektor a1 a2 a3) (Vektor b1 b2 b3) = Vektor (a1+b1) (a2+b2) (a3+b3) 

    vektorMultiplikation :: (Num t) => t -> Vektor t -> Vektor t
    vektorMultiplikation x (Vektor a b c) = Vektor (x*a) (x*b) (x*c)

    vektorSkalarprodukt :: (Num t) => Vektor t -> Vektor t -> t
    vektorSkalarprodukt (Vektor a1 a2 a3) (Vektor b1 b2 b3) = a1*b1 + a2*b2 + a3*b3