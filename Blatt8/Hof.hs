import Prelude hiding (map, any, all, iterate, filter, takeWhile, zipWith, curry, uncurry, (.), foldr, foldl, foldl1, foldr1)
import Data.Char ()

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x:xs) = f x && all f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs)
    |f x = x : takeWhile f xs
    |otherwise = []

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
    |f x = x : filter f xs
    |otherwise = filter f xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f [] = ([], [])
partition f (x:xs)
    | f x = (x : links (partition f xs), rechts (partition f xs))
    |otherwise = (links (partition f xs), x : rechts (partition f xs))
        where 
            links (p_1, p_2) = p_1
            rechts (p_1, p_2) = p_2

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] b = []
zipWith f a [] = []
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

foldl :: (acc -> el -> acc) -> acc -> [el] -> acc
foldl _ acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (el -> acc -> acc) -> acc -> [el] -> acc
foldr _ acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [] = error "Liste ist Leer"
foldr1 f h@(x:xs) = foldr f (last h)(init h)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f [] = error "Liste ist Leer"
foldl1 f (x:xs) = foldl f x xs

alleAnwenden :: [arg -> wert] -> arg -> [wert]
alleAnwenden [] _ = []
alleAnwenden (f:fs) x = f x : alleAnwenden fs x

hintereinanderAnwenden :: [wert -> wert] -> wert -> wert
hintereinanderAnwenden fs x = foldl (\ x f -> f x) x fs