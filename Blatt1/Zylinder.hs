-- VolumenRadiusHoehe
volumenAusRadiusUndHoehe :: Double -> Double -> Double
volumenAusRadiusUndHoehe r h =
    if r < 0 || h < 0
    then error "keine Zahlen unter 0 verwenden"
    else    pi * (r**2) * h 

-- RadiusVolumenHoehe
radiusAusVolumenUndHoehe :: Double -> Double -> Double
radiusAusVolumenUndHoehe v h 
    | v < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = sqrt (v / (pi * h))

-- HoeheVolumenRadius
hoeheAusVolumenUndRadius :: Double -> Double -> Double
hoeheAusVolumenUndRadius v r 
    | v < 0 || r < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = v / (pi * r^2)

-- OberflaecheRadiusHoehe
oberflaecheAusRadiusUndHoehe :: Double -> Double -> Double
oberflaecheAusRadiusUndHoehe r h
    | r < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = (2 * pi * (r^2)) + (2 * pi * r * h)

-- RadiusOberflaecheHoehe
radiusAusOberflaecheUndHoehe :: Double -> Double -> Double
radiusAusOberflaecheUndHoehe o h 
    | o < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = (-h/2) + sqrt((h/2 * h/2) + (o/2 /pi))

-- HoeheOberflaecheRadius
hoeheAusOberflaecheUndRadius :: Double -> Double -> Double
hoeheAusOberflaecheUndRadius o r
    | o < 0 || r < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = if o < 2 * pi * (r^2) 
                then error "Oberfläche ist zu klein"
                else (o / (2 * pi * r)) -r

-- VolumenOberflaecheRadius
volumenAusOberflacheUndRadius :: Double -> Double -> Double
volumenAusOberflacheUndRadius o r
    | o < 0 || r < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = volumenAusRadiusUndHoehe r (hoeheAusOberflaecheUndRadius o r)

-- VolumenOberflaecheHoehe
volumenAusOberflacheUndHoehe :: Double -> Double -> Double
volumenAusOberflacheUndHoehe o h
    | o < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = (o / (2 * pi * r)) -r 
    where r = radiusAusOberflaecheUndHoehe o h

-- OberflaecheVolumenRadius
oberflaecheAusVolumenUndRadius :: Double -> Double -> Double
oberflaecheAusVolumenUndRadius v r
    | v < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = 2 * pi * r * (r + h)
    where h = hoeheAusVolumenUndRadius v r

-- OberflaecheVolumenHoehe
oberflaecheAusVolumenUndHoehe :: Double -> Double -> Double
oberflaecheAusVolumenUndHoehe v h
    | v < 0 || h < 0  = error "keine Zahlen unter 0 verwenden"
    | otherwise = 2 * pi * r * (r + h)
    where r = radiusAusVolumenUndHoehe v h