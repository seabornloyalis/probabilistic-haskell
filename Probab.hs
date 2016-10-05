--ProbabFP.hs
--Author: Chad Myles
--Date: 9/26/16

module Probab (
    Dist, 
    unit,
    uniformDist,
    weightedDist,
    toList,
    mergeEqual,
    possibilities,
    probabilityOf,
    adjoin,
    distFil,
    transform,
    combine,
    duplicate
    ) where

import Data.List

data Dist a = Dist [(a, Float)]
    deriving( Show)

instance Functor Dist where
    fmap f (Dist xs) = Dist (map (\(x,p) -> (f x, p)) xs)

--TODO: make sure sum of Floats = 1 and none are negative

--
-- Public Interface
--

unit :: a -> Dist a

uniformDist :: [a] -> Dist a

weightedDist :: [(a, Float)] -> Dist a

toList :: Dist a -> [(a, Float)]

mergeEqual :: Eq a => Dist a -> Dist a

possibilities :: Eq a => Dist a -> [a]

probabilityOf :: Eq a => Dist a -> (a -> Bool) -> Float

adjoin :: (a -> Dist b) -> Dist a -> Dist (a,b)

distFil :: (a -> Bool) -> Dist a -> Dist a

transform :: (a -> Dist b) -> Dist a-> Dist b

combine :: Dist a -> Dist b -> Dist (a, b)

duplicate :: Integral a => a -> Dist b -> Dist [b]
--
-- Implementation of Public Interface
--

--unit :: a -> Dist a
unit x = Dist [(x,1.0)]

--uniformDist :: [a] -> Dist a
uniformDist xs = Dist (map (\ x -> (x, (1.0 / len))) xs)
    where len = fromIntegral (length xs)

--weightedDist :: [(a, Float)] -> Dist a
weightedDist xs = Dist xs

--toList :: Dist a -> [(a, Float)]
toList (Dist valList ) = valList

--mergeEqual :: Eq a => [(a, Float)] -> [(a, Float)]
mergeEqual xs =
    let distinct = possibilities xs
    in weightedDist (map (\y -> (y, (foldl (\acc (b,c) ->if (b == y)
                                                        then (acc + c)
                                                        else acc) 0.0 (toList xs)))) distinct)

--possibilites :: Eq a => Dist a -> [a]
possibilities xs =
    let firsts = map (\(a,b) -> a) (toList xs)
    in nub firsts

--probabilityOf :: Eq a => [(a, Float)] -> a -> Float
probabilityOf xs f = 
    foldl (\acc (a, b) -> if (f a)
                            then (acc + b)
                            else (acc)) 0.0 (toList (mergeEqual xs))  

--adjoin :: (a -> Dist b) -> Dist a -> Dist (a,b)
adjoin f (Dist xs) =
            let wrapped = map (\(k, p) ->
                            let (Dist res) = f k
                            in map (\(k',p') -> ((k,k'), p*p'))
                                res)
                                    xs
            in Dist (concat wrapped)

--distFil :: (a -> Bool) -> Dist a -> Dist a
distFil f xs =
    let intermed = filter (\ x -> f (fst x)) (toList xs)
    in weightedDist (map (\ (x,y) -> (x, y * (1.0 / totalProb(intermed)))) intermed)

--transform :: (a -> Dist b) -> Dist a -> Dist b
transform f xs = 
    --create ([(b, Float)], Float) list
    let intermed = (map (\(a, b) -> ((toList (f a)), b)) (toList xs))
    in weightedDist (concat (map (\(a, b) -> (map (\(c,d) -> (c, b*d)) a)) intermed))

--combine :: Dist a -> Dist b -> Dist (a, b)
combine xs ys = weightedDist (concat (map (\ x -> map (\ y -> ((fst x, fst y), snd x * snd y)) (toList ys)) (toList xs)))

--duplicate :: Dist a -> Int -> Dist [a]
duplicate num xs = if (num == 1)
                    then weightedDist (map (\(a,b) -> ([a], b)) (toList xs))
                    else weightedDist (map (\((a,b),c) -> (a:b,c)) (toList (combine xs (duplicate (num-1) xs))))


--
-- Private functions
--
totalProb :: [(a,Float)] -> Float
totalProb xs = foldl (\acc (a,b) -> acc + b) 0.0 xs

