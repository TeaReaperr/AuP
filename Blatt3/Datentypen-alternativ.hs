--Bruch
data Bruch = Bruch Int Int deriving Show
nenner :: Bruch -> Int
nenner (Bruch a _) = a

zaehler :: Bruch -> Int
zaehler (Bruch _ b) = b

--Datum
data Datum = Datum Int Int Int deriving Show
tag :: Datum -> Int
tag (Datum a _ _) = a

monat :: Datum -> Int
monat (Datum _ b _) = b

jahr :: Datum -> Int
jahr (Datum _ _ c) = c

--Uhrzeit
data Uhrzeit = Uhrzeit Int Int Int deriving Show
stunde :: Uhrzeit -> Int
stunde (Uhrzeit a _ _) = a

minute :: Uhrzeit -> Int
minute (Uhrzeit _ b _) = b

sekunde :: Uhrzeit -> Int
sekunde (Uhrzeit _ _ c) = c

--Kasse
data Kasse = Kasse String String deriving Show
kassenId :: Kasse -> String
kassenId (Kasse a _) = a

kassiererName :: Kasse -> String
kassiererName (Kasse _ b) = b

--Mensaessen
data Mensaessen = Tagessuppe | Essen1 | Essen2 | Bioessen | Vegetarisch | Aktionsessen deriving Show

--Preis?
data Preis = Preis Int Int deriving Show

--Kassenbon
data Kassenbon = Kassenbon Kasse Datum Mensaessen Preis deriving Show
kasse :: Kassenbon -> Kasse
kasse (Kassenbon a _ _ _) = a

datum :: Kassenbon -> Datum
datum (Kassenbon _ b _ _) = b

mensaessen :: Kassenbon -> Mensaessen
mensaessen (Kassenbon _ _ c _) = c

preis :: Kassenbon -> Preis
preis (Kassenbon _ _ _ d) = d