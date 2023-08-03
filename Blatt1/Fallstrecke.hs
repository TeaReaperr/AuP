-- Aufgabe 3.1
-- weg ::               double
-- Zeit ::              double
-- Beschleunigung ::    double

-- Aufgabe 3.2
fallstrecke ::Double -> Double
fallstrecke t = (1/2) * 9.81 * (t**2)

-- Aufgabe 3.3
globalesG :: Double
globalesG = 9.81

fallstreckeGlobal :: Double -> Double
fallstreckeGlobal t = 1/2 * globalesG * t**2

--Aufgabe 3.4
fallstreckeWhere :: Double -> Double
fallstreckeWhere t = 
    (1/2) * g * (t**2)
        where
    g = 9.81

--Aufgabe 3.5
fallstreckeLet :: Double -> Double
fallstreckeLet t =
    let 
        g = 9.81
    in
        (1/2) * g * (t**2)

--Aufgabe 3.6
fallstreckeIf :: Double -> Double
fallstreckeIf t =
    if t < 0 then
        error "Negative Zahl"
    else
        (1/2) * g * (t**2)
            where 
        g = 9.81

fallstreckeGuards :: Double -> Double
fallstreckeGuards t | t < 0 = error "Negative Zahl"
                    |  otherwise = (1/2) * g * (t^2) where g = 9.81