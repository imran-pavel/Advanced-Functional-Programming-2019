
{-|

Some examples of how to run it:
- fmap (+2) Null
- fmap (+2) (Value 5)
- fmap (++ " da Firenze") (Value "Ezio Auditore")

-}




{-|

Make your Null  a functor.

-}


data MaybeNull a = Value a | Null deriving (Show, Eq)


instance Functor MaybeNull where
    fmap f Null = Null
    fmap f (Value a) = Value (f a)


    