-- To try it out do the followings:
-- :t Value 'g'   or
-- :t Value 5     or
-- :t Null        or
-- :t Value [1,2,3]



{-|

Database nulls are different from Nothing in that Nothing == Nothing but in queries Null /= Null.

Make a type constructor MaybeNull (like database Null) which either contains a value or Null.

-}


data MaybeNull a = Value a | Null deriving (Show)