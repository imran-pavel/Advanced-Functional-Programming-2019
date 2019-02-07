
{-|

For this 'Unknown' return type I made a custom bool data type, since standard bool has only True and False (as far as I am concerned!).
 (=&=) and (/&=) are the 3 valued equality functions.
- (=&=) checks if values are equal (if not null)
- (/&=) checks if values are not equal (if not null)
 
 Run like any of these:
 - (=&=) (Value [2,3,4]) Null
 - (=&=) (Value [2,3,4]) (Value [2,3,4])
 - (/&=) Null (Value [2,3,4])
 - (/&=) (Value [2]) (Value [2,3,4])
 - (=&=) (Value (Just 5)) (Value (Just 2))




- (/&=) (Value x) (Value y)  or  (=&=) (Value x) (Value y)
    - Will return CTrue or CFalse as long as both of x and y are members of Eq

- (=&=) Null (Value x)  or  (=&=) (Value x) Null
    - Will always return Unknown ( Same for (/&=) )

-}




{-|

Define a typeclass for 3-valued equality (True, False, Unknown). Make a type constructor MaybeNull (like database Null) which either contains a value or Null.
Make MaybeNull a member of your three-valued equality (comparison of Null with something would give Unknown).

-}





data CustomBool = Unknown | CFalse | CTrue deriving (Show)

class Eq3 a where
    (=&=) :: a -> a -> CustomBool
    (/&=) :: a -> a -> CustomBool

data MaybeNull a = Value a | Null deriving (Show, Eq)


truthCheck :: (Eq a) => a -> a -> CustomBool
truthCheck x y
    | x == y = CTrue
    | otherwise = CFalse


falseCheck :: (Eq a) => a -> a -> CustomBool
falseCheck x y
    | x /= y = CTrue
    | otherwise = CFalse


instance (Eq a) => Eq3 (MaybeNull a) where
    (=&=) Null _ = Unknown
    (=&=) _ Null = Unknown
    (=&=) (Value x) (Value y) = truthCheck x y
    (/&=) Null _ = Unknown
    (/&=) _ Null = Unknown
    (/&=) (Value x) (Value y) = falseCheck x y
    

    