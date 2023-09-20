data Tree a b = Leaf b
    | Branch (Tree a b) a (Tree a b) deriving Show

getLeaves :: Tree a b -> [b]
getLeaves (Leaf b) = [b]
getLeaves (Branch x a y) = getLeaves x ++ getLeaves y

bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
bimap f g (Leaf x) = Leaf(g x)
bimap f g (Branch x a y) = Branch (bimap f g x) (f a) (bimap f g y)