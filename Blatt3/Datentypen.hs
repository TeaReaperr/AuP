--Produkttypen
--data T = K a1 ... an

--Summentyp
--data T = K1 a1 | ... | Kn an
--data Person == Student String | Mitarbeiter String

--foo :: Person -> String
--foo :: Student name = ...
--foo Mitarbeiter name = ...

--Kombination
--data T = K1 a1 ... a1kn | ... | Kn an 

--Aufzähltypen
--data Mensch = Student | Lehrer | Eltern


--Aufgabe 2

data Punkt = Punkt {xKoord :: Double, yKoord :: Double} deriving Show
--Produkttyp
--Punkt :: Double -> Double -> Punkt
--xKoord :: Punkt -> Double
--yKoord :: Punkt -> Double

data Bruch = Bruch {zaehler :: Int, nenner :: Int} deriving Show
--Produkttyp
--Bruch :: Int -> Int -> Bruch
--zaehler :: Bruch -> Int
--nenner :: Bruch -> Int

data Bewertung = Bestanden | NichtBestanden | Nachzuarbeiten deriving Show
--Aufzähltyp
--Bestanden :: Bewertung
--NichtBestanden :: Bewertung
--Nachzuarbeiten :: Bewertung

data Wochentag =  Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag deriving Show
--Aufzähltyp
--Montag :: Wochentag
--Dienstag :: Wochentag
--Mittwoch :: Wochentag
--Donnerstag :: Wochentag
--Freitag :: Wochentag
--Samstag :: Wochentag
--Sonntag :: Wochentag

data Monat = Jannuar | Februar | Maerz | April | Mai | Juni | Juli | August | September | Oktober | November | Dezember deriving Show
--Aufzähltyp
--Jannuar :: Monat
--Februar :: Monat
--März :: Monat
--April :: Monat
--Mai :: Monat
--Juni :: Monat
--Juli :: Monat
--August :: Monat
--September :: Monat
--Oktober :: Monat
--Novvember :: Monat
--Dezember :: Monat

data Datum = Datum {tag :: Int, monat :: Int, jahr :: Int} deriving Show
--Produkttyp
--Datum :: Int -> Int -> Int -> Datum
--tag :: Datum -> Int
--monat :: Datum -> Int
--jahr :: Datum -> Int

data Uhrzeit = Uhrzeit {stunde :: Int, minute :: Int, sekunde :: Int} deriving Show
--Produkttyp
--Uhrzeit :: Int -> Int -> Int -> Uhrzeit
--stunde :: Uhrzeit -> Int
--minute :: Uhrzeit -> Int
--sekunde :: Uhrzeit -> Int

data Preis = Preis {euro :: Int, cent :: Int} deriving Show
--Produkttyp
--Preis :: Int -> Int -> Preis
--euro :: Preis -> Int
--cent :: Preis -> Int

data Kasse = Kasse {kassenId :: String, kassiererName :: String} deriving Show
--Produkttyp
--Kasse :: String -> String -> Kass
--kassenId :: Kasse -> String
--kassierername :: Kasse -> String

data Mensaessen = Tagessuppe | Essen1 | Essen2 | Bioessen | Vegetarisch | Aktionsessen deriving Show
--Aufzähltyp
--Tagessuppe :: Mensaessen
--Essen1 :: Mensaessen
--Essen2 :: Mensaessen
--Bioessen :: Mensaessen
--Vegetarisch :: Mensaessen
--Aktionsessen :: Mensaessen

data Umzugskarton = Umzugskarton {volumen :: Double, maximalgewicht :: Double} deriving Show
--Produkttyp
--Umzugskarton :: Double -> Double -> Umzugskarton
--volumen :: Umzugskarton -> Double
--maximalgewicht :: Umzugskarton -> Double

data Kassenbon = Kassenbon {kasse :: Kasse, datum :: Datum, mensaessen :: Mensaessen, preis :: Preis} deriving Show
--Produkttyp
--Kassenbon :: Kasse -> Datum -> Mensaessen -> Preis -> Kassenbon
--kasse :: Kassenbon -> Kasse
--datum :: Kassenbon -> Datum
--mensaessen :: Kassenbon -> Mensaessen
--preis :: Kassenbon -> Preis
--Beispiel: Kassenbon (Kasse "d112432" "OwO") (Datum 10 04 2023) (Essen1) (Preis 2 60)

kassiererVonKassenbon :: Kassenbon -> String
kassiererVonKassenbon kb = kassiererName (kasse kb)

jahrVomKassenbon :: Kassenbon -> Int
jahrVomKassenbon kb = jahr (datum kb)

preisVonKassenbon :: Kassenbon -> Preis
preisVonKassenbon = preis

tagVonKassenbon :: Kassenbon -> Int
tagVonKassenbon kb = tag (datum kb)