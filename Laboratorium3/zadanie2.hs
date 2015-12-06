{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

-- zadanie 2
class Stackable a v | a -> v where
    empty :: a
    push  :: v -> a -> a
    pop   :: a -> (v, a)

data Stack a = Empty | Element a (Stack a) deriving Show

instance Stackable (Stack v) v where
    empty = Empty

    push val Empty = Element val Empty
    push val (Element e s) = Element val (Element e s)

    pop Empty = error "No elements left"
    pop (Element e s) = (e,s)