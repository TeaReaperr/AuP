data Ausdruck = Konstante Integer
    | Variable String 
    | Summe Ausdruck Ausdruck
    | Differenz Ausdruck Ausdruck
    | Produkt Ausdruck Ausdruck
    | Quotient Ausdruck Ausdruck
    deriving Show

--1.
--a) Summe (Konstante 12) (Variable "z")
--b) Summe (Produkt (variable "x")(Variable "z") Konstante 7)
--c) Produkt (Differenz (Konstante 8) (Variable "x")) (variable "y")
--d) Quotient (Differenz (Variable "z") (Variable "j")) (Konstante 5)
--2.
--a) 3+x
--b) (x+3)+(4/y)
--c) (k*8)-1

--3.
ausdruckNachString :: Ausdruck -> String
ausdruckNachString (Konstante x) = show x
ausdruckNachString (Variable x) = x
ausdruckNachString (Produkt x y) = "(" ++ ausdruckNachString x ++ "*" ++ ausdruckNachString y ++ ")"
ausdruckNachString (Quotient x y) = "(" ++ ausdruckNachString x ++ "/" ++ ausdruckNachString y ++ ")"
ausdruckNachString (Summe x y) = "(" ++ ausdruckNachString x ++ "+" ++ ausdruckNachString y ++ ")"
ausdruckNachString (Differenz x y) = "(" ++ ausdruckNachString x ++ "-" ++ ausdruckNachString y ++")"

belegungVonVariable :: String -> [(String, Integer)] -> Integer
belegungVonVariable _ [] = error "keine Belegung"
belegungVonVariable x ((y, z) : liste) =
    if x == y then z
    else belegungVonVariable x liste

auswerten :: Ausdruck -> [(String, Integer)] -> Integer
auswerten _ [] = error "keine Belegung"
auswerten (Konstante x) _ = x
auswerten (Variable x) y = belegungVonVariable x y
auswerten (Produkt x y) z = auswerten x z * auswerten y z
auswerten (Quotient x y) z = auswerten x z `div` auswerten y z 
auswerten (Summe x y) z = auswerten x z + auswerten y z 
auswerten (Differenz x y) z = auswerten x z - auswerten y z 