data Tree a = Empty
    | Node a (Tree a) (Tree a)

max :: Int -> Int -> Int
max a b | a > b = a
    | otherwise = b

anzahlKnoten :: Tree a -> Int
anzahlKnoten Empty = 0
anzahlKnoten (Node _ l r) = 1 + anzahlKnoten l + anzahlKnoten r

depth :: Tree a -> Int
depth Empty = 1
depth (Node x links rechts) = 1 + Main.max (depth links) (depth rechts)

{-
T(Empty) = 1  -> Terminiert

T(Node x links rechts)
auf der rechten Seite wird bei der zweiten Eingabe nur noch mit depth links und depth rechts gearbeitet,
was <= depth von dem Tree ist und somit terminiert diese Seite.
Also gilt max depth links/rechts ist mindestens 1 kleiner als die max depth vom Baum.

meow
(N, <=)
-}