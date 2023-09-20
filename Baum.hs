data Tree a b = Leaf b
    | Branch (Tree a b) a (Tree a b)

getLeaves :: Tree a b -> [b]
getLeaves (Leaf b) = [b]
getLeaves (Branch x a y) = getLeaves x ++ getLeaves y

