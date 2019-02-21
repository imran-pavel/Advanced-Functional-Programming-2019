{-|

Check the comments below to test out the 'routines'.

-}


{-|

Make a Null your Null type constructor a Monad. Write a two different routines using do notation to try out the Monadic properties of your Null.

-}

import Control.Monad


data MaybeNull a = Null | Value a deriving (Show, Eq)


instance Functor MaybeNull where
    fmap f Null = Null
    fmap f (Value a) = Value (f a)


instance Applicative MaybeNull where
    pure x = Value x
    Null <*> _ = Null
    Value f <*> x = fmap f x


instance Monad MaybeNull where
    return x = Value x
    Null >>= _ = Null
    Value x >>= f = f x
    fail _ = Null





-- routine1 is equivalent to : Value 100 >>= return
-- From book: m >>= return is no different than m. So routine1 output will be Value 100.
routine1 :: MaybeNull Int
routine1 = do
                x <- Value 100
                return (x+1)



-- routine2 is equivalent to : return 100 >>= f     -- f is everything after line 55 inside routine2. (Not to be confused with the f just above routine2Check)
                                                    -- I didn't add anything other than one return after line 55 to keep it simple. )
-- From book: return x >>= f should be equal to f x. Run routine2 and then routine2Check in terminal.
routine2 :: MaybeNull Int
routine2 = do
                x <- return 100
                return (x+1)


f = (\x -> return (x+1) :: MaybeNull Int)        
routine2Check = f 100


-- Third law check??
fn1 :: Int -> Maybe Int
fn1 x = return (x*1)

fn2 :: Int -> Maybe Int
fn2 x = return (x*10)

fn3 :: Int -> Maybe Int
fn3 x = return (x*100)

-- From the book: The third law states that f <=< (g <=< h) should be the same as (f <=< g) <=< h.
-- In terminal try: testFn1 5  and testFn2 5
testFn1 = (fn1 <=< fn2) <=< fn3
testFn2 = fn1 <=< (fn2 <=< fn3)