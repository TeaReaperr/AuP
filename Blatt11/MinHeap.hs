module MinHeap (
    MinHeap,
    emptyMinHeap,
    heapEmpty,
    --checkMinHeapProperty,
    --minElem,
    --insMinHeap,
    --delMinHeap
) where

    data MinHeap a = MinHeap [a] deriving Show

    emptyMinHeap :: Ord a => MinHeap a
    emptyMinHeap = MinHeap []

    heapEmpty :: Ord a => MinHeap a -> Bool
    heapEmpty (MinHeap []) = True
    heapEmpty (MinHeap _) = False

    --checkMinHeapProperty :: Ord a => MinHeap a -> Bool
    
    --minElem :: Ord a => MinHeap a -> a
    
    --insMinHeap :: Ord a => MinHeap a -> MinHeap a
    
    --delMinHeap :: Ord a => MinHeap a -> MinHeap a

    --swap :: Int -> Int -> [a] -> [a]