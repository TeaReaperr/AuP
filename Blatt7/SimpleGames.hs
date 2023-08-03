import Karten

testCardCreation :: Karte
testCardCreation = Karte Herz Ass

testStapel :: Kartenstapel
testStapel = neuerKartenstapel

countCards :: Kartenstapel -> Int
countCards [] = 0
countCards h@(x:xs) = y + q
    where
            y = wCards(getWert x) + countCards xs
            z = aCards h
            q = if y + z * 10 <= 21 then z * 10 else 0

aCards :: Kartenstapel -> Int
aCards [] = 0
aCards ((Karte _ Ass):xs) = wCards Ass + aCards xs
aCards (x:xs) = aCards xs

wCards :: Wert -> Int
wCards Ass = 1
wCards Bube = 10
wCards Dame = 10
wCards Koenig = 10
wCards Zwei = 2
wCards Drei = 3
wCards Vier = 4
wCards Fuenf = 5
wCards Sechs = 6
wCards Sieben = 7
wCards Acht = 8
wCards Neun = 9
wCards zehn = 10

testCountCards1 :: Bool
testCountCards1 = 21 == countCards [Karte Herz Ass, Karte Kreuz Koenig, Karte Pik Dame]
testCountCards2 :: Bool
testCountCards2 = 21 == countCards [Karte Herz Ass, Karte Kreuz Koenig]
testCountCards3 :: Bool
testCountCards3 = 12 == countCards [Karte Herz Ass, Karte Kreuz Ass, Karte Pik Dame]
testCountCards4 :: Bool
testCountCards4 = 28 == countCards [Karte Herz Zehn, Karte Kreuz Zehn, Karte Pik Acht]
testCountCards5 :: Bool
testCountCards5 = 18 == countCards [Karte Karo Vier, Karte Karo Sechs, Karte Karo Acht]
testCountCards6 :: Bool
testCountCards6 = 40 == countCards [Karte Herz Dame, Karte Pik Dame, Karte Kreuz Dame, Karte Karo Dame]
testCountCards7 :: Bool
testCountCards7 = 23 == countCards [Karte Herz Dame, Karte Pik Dame]

{-
containsFullHouse :: Kartenstapel -> Bool
containsFullHouse (x:xs) =
    if
-}