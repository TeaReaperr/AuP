--Geometrie
data Gerade = Gerade {m :: Double,  n :: Double} deriving Show
data Punkt = Punkt {x:: Double, y :: Double} deriving Show

auswerten :: Gerade -> Double -> Double
auswerten (Gerade m n ) x = m * x + n

schnittpunkt :: Gerade -> Gerade -> Punkt
schnittpunkt (Gerade mx nx) (Gerade my ny)
  | mx == my = error "Bei gleichem Anstieg gibt es keinen Schnittpunkt"
  | otherwise = Punkt x y
    where x = (ny - nx) / (mx - my)
          y = auswerten (Gerade mx nx ) x

flaecheZwischenGeraden :: Gerade -> Gerade -> Double -> Double -> Double
flaecheZwischenGeraden f@(Gerade mx nx) g@(Gerade my ny) a b
  | a >= b = 0
  | a >= x || b <= x = abs(((1 / 2 ) * (my - mx) * a * a) + ((ny - nx) * a)) - (((1 / 2) * (my - mx) * b * b) + ((ny -nx) * b))
  | otherwise = flaecheZwischenGeraden f g a x + flaecheZwischenGeraden f g x b
  where 
    Punkt x y = schnittpunkt f g

--flaecheZwischenGeraden (Gerade (-0.5) 3) (Gerade 0.5 0) 0 6