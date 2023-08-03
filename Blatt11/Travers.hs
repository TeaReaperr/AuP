class Sequenz sequenz where
    leer :: sequenz el
    einfuegen :: el -> sequenz el -> sequenz el
    loeschen :: sequenz el -> sequenz el
    naechstesElement :: sequenz el -> el
    istLeer :: sequenz el -> Bool

data Stack el = Stack [el] deriving Show
instance Sequenz Stack where
    leer :: Stack el
    leer = Stack []

    einfuegen :: el -> Stack el -> Stack el
    einfuegen e (Stack es) = Stack (e:es) 
    
    loeschen :: Stack el -> Stack el
    loeschen (Stack(x:xs)) = Stack xs 
    
    naechstesElement :: Stack el -> el
    naechstesElement (Stack (x:xs)) = x
    
    istLeer :: Stack el -> Bool
    istLeer (Stack []) = True
    istLeer (Stack _) = False

data Queue el = Queue [el] deriving Show
instance Sequenz Queue where
    leer :: Queue el
    leer = Queue []
    
    einfuegen :: el -> Queue el -> Queue el
    einfuegen e (Queue es) = Queue (es ++ [e])
    
    loeschen :: Queue el -> Queue el
    loeschen (Queue (x:xs)) = Queue xs
    
    naechstesElement :: Queue el -> el
    naechstesElement (Queue (x:xs)) = x
    
    istLeer :: Queue el -> Bool
    istLeer (Queue []) = True
    istLeer (Queue _) = False