module Karten (
    Karte(..),
    Wert(..),
    Farbe(..),
    Kartenstapel(..),
    getWert,
    getFarbe,
    neuerKartenstapel,
    geben
) where
    data Karte = Karte {farbe :: Farbe, wert :: Wert} deriving Show
    
    data Farbe = Herz | Karo | Kreuz | Pik 
    
    instance Eq Farbe where 
        (==) Karo Karo = True
        (==) Herz Herz = True
        (==) Kreuz Kreuz = True
        (==) Pik Pik = True
    
    getFarbe :: Karte -> Farbe
    getFarbe (Karte farbe _) = farbe

    data Wert = Bube | Dame | Koenig | Ass | Zwei | Drei | Vier | Fuenf | Sechs | Sieben | Acht | Neun | Zehn 

    instance Eq Wert where
        (==) Bube Bube = True
        (==) Dame Dame = True
        (==) Koenig Koenig = True
        (==) Ass Ass = True
        (==) Zwei Zwei = True
        (==) Drei Drei = True
        (==) Vier Vier = True
        (==) Fuenf Fuenf = True
        (==) Sechs Sechs = True
        (==) Sieben Sieben = True
        (==) Acht Acht = True
        (==) Neun Neun = True
        (==) Zehn Zehn = True
    
    getWert :: Karte -> Wert
    getWert (Karte _ wert) = wert

    instance Eq Karte 
        where (==) :: Karte -> Karte -> Bool
              Karte f1 w1 == Karte f2 w2 = f1 == f2 && w1 == w2

    instance Show Farbe
        where 
            show Herz = "Herz"
            show Karo = "Karo"
            show Pik = "Pik"
            show Kreuz = "Kreuz"

    instance Show Wert
        where
            show Bube = "Bube"
            show Dame = "Dame"
            show Koenig = "Koenig"
            show Ass = "Ass"
            show Zwei = "Zwei"
            show Drei = "Drei"
            show Vier = "Vier"
            show Fuenf = "Fünf"
            show Sechs = "Sechs"
            show Sieben = "Sieben"
            show Acht = "Acht"
            show Neun = "Neun"
            show Zehn = "Zehn"

    type Kartenstapel = [Karte]

    neuerKartenstapel :: Kartenstapel
    neuerKartenstapel = [Karte b d | b <- f, d <- w]
        where
            f = [Herz, Karo, Kreuz, Pik]
            w = [Zwei, Drei, Vier, Fuenf, Sechs, Sieben, Acht, Neun, Zehn, Bube, Dame, Koenig, Ass]
    
    geben :: Int -> Int -> Kartenstapel -> [Kartenstapel]
    geben 0 _ _ = []
    geben _ 0 _ = []
    geben _ _ _ = error "leerer Stapel"
    --geben x y (z:zs) = x 1 z : geben (x-1) (y-1) zs