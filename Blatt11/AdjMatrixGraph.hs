module AdjMatrixGraph (
    Graph,
    beispielGraph,
    graph,
    knoten,
    kanten,
    gewichteteKanten,
    adjazenteKnoten,
    istKante,
    gewicht
) where

import Data.Maybe
import Data.Array

{-
    Gerichtete Graphen.
-}
data Graph knoten gewicht = Graph (Array (knoten,knoten) (Maybe gewicht))

instance (Ix knoten, Show knoten, Show gewicht) => Show (Graph knoten gewicht) where

    show graph = "graph " ++ show (knotenIntervall graph) ++ " " ++ show (gewichteteKanten graph)

{-
    Ein Beispielgraph zum Testen der nachstehenden Operationen.
-}
beispielGraph :: Graph Int Double
beispielGraph = graph (1,5) [
                                (1,3,1.1),
                                (1,4,2.2),
                                (2,1,3.3),
                                (3,2,4.4),
                                (3,4,5.5),
                                (4,2,6.6),
                                (4,5,7.7),
                                (5,3,8.8)
                            ]

{-
    Konstruiert einen gerichteten Graphen aus einem Knotenintervall und einer Liste gewichteter
    Kanten.
-}
graph :: (Ix knoten) => (knoten,knoten) -> [(knoten,knoten,gewicht)] -> Graph knoten gewicht
graph  (minKnoten, maxKnoten) gewichteteKanten = Graph matrix where
        matrix = accumArray(const Just) Nothing ((minKnoten, minKnoten), (maxKnoten, maxKnoten)) (map assosziation gewichteteKanten)
        
        assosziation :: (knoten, knoten, gewicht) -> ((knoten, knoten), gewicht)
        assosziation (knoten1, knoten2, gewicht) = ((knoten1, knoten2), gewicht)        

{-
    Liefert das Knotenintervall.
-}
knotenIntervall :: (Ix knoten) => Graph knoten gewicht -> (knoten,knoten)
knotenIntervall (Graph matrix) = let ((minKnoten, _), (maxKnoten, _)) = bounds matrix
                in (minKnoten, maxKnoten)

{-
    Liefert eine Liste aller Knoten.
-}
knoten :: (Ix knoten) => Graph knoten gewicht -> [knoten]
knoten = undefined

{-
    Liefert eine Liste aller Kanten.
-}
kanten :: (Ix knoten) => Graph knoten gewicht -> [(knoten,knoten)]
kanten = undefined

{-
    Liefert eine Liste aller Kanten mit Gewichten.
-}
gewichteteKanten :: (Ix knoten) => Graph knoten gewicht -> [(knoten,knoten,gewicht)]
gewichteteKanten = undefined

{-
    Berechnet die zu einem Knoten adjazenten Knoten.
-}
adjazenteKnoten :: (Ix knoten) => Graph knoten gewicht -> knoten -> [knoten]
adjazenteKnoten = undefined

{-
    Prüft, ob zwischen zwei gegebenen Knoten eine Kante existiert.
-}
istKante :: (Ix knoten) => (knoten,knoten) -> Graph knoten gewicht -> Bool
istKante = undefined

{-
    Ermittelt das Gewicht einer Kante.
-}
gewicht :: (Ix knoten) => Graph knoten gewicht -> (knoten,knoten) -> gewicht
gewicht = undefined
