{-|
Write a function that take as parameters:
- a function f that, given a pair of two Float coordinate points, calculates their distance,
- a Float coordinate point p, and
- a distance d (also Float),
- a list ps of points (pairs of Float)
calculates a list of such points of ps that their distance (using  f) from p is at most d.

For instance if f calculates the Euclidian distance, p is (1.0,1.0), d is 2, and ps is [(1.0,2.0),(3.0,3.0)] then the answer is [(1.0,2.0)]
After that, use partial evaluation to create another function, where f has been fixed to the Manhattan distance function, by using partial evaluation.
Do that by fixing f with a lambda funciton. Manahttan distance is the x-distance + y-distance between the points.
E.g. Manhattan distance between (2.0,1.0) and (3.0,3.0) is 1.0+2.0 = 3.0.
-}


{-|

Call like this:    solution euclidDis (1.0,1.0) 2 [(1.0,2.0), (3.0, 3.0)]

-}


euclidDis :: (Float, Float) -> (Float, Float) -> Float
euclidDis (x1, y1) (x2, y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)



manhatDis :: (Float, Float) -> (Float, Float) -> Float
manhatDis (x1, y1) (x2, y2) = abs(x1-x2) + abs(y1-y2)


solution :: ((Float, Float) -> (Float, Float) -> Float) -> (Float, Float) -> Float -> [(Float, Float)] -> [(Float, Float)]
solution f p d ps = filter (\currentP -> (f p currentP) <= d) ps


solution2 :: (Float, Float) -> Float -> [(Float, Float)] -> [(Float, Float)]
solution2 p d ps = filter (\currentP -> ((pFunction p currentP) <= d)) ps


pFunction :: (Float, Float) -> (Float, Float) -> Float
pFunction  = (\f p currentP -> f p currentP ) manhatDis