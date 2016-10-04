--Probab.hs
--Author: Chad Myles
--Date: 9/26/16

import Data.List

data Dist a = Dist [(a,Rational)]
    deriving( Show)

instance Functor Dist where
    fmap f (Dist xs) = Dist (map (\(x,p) -> (f x, p)) xs)

--TODO: make sure sum of Rationals = 1 and none are negative

--
-- Public Interface
--

unit :: a -> Dist a

uniformDist :: [a] -> Dist a

weightedDist :: [(a, Rational)] -> Dist a

toList :: Dist a -> [(a, Rational)]

mergeEqual :: Eq a => Dist a -> Dist a

possibilities :: Eq a => Dist a -> [a]

probabilityOf :: Eq a => Dist a -> (a -> Bool) -> Rational

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

--weightedDist :: [(a, Rational)] -> Dist a
weightedDist xs = Dist xs

--toList :: Dist a -> [(a, Rational)]
toList (Dist valList ) = valList

--mergeEqual :: Eq a => Dist a -> Dist a
mergeEqual xs =
    let distinct = possibilities xs
    in weightedDist (map (\y -> (y, (foldl (\acc (b,c) ->if (b == y)
                                                        then (acc + c)
                                                        else acc) 0.0 (toList xs)))) distinct)

--possibilites :: Eq a => Dist a -> [a]
possibilities xs =
    let firsts = map (\(a,b) -> a) (toList xs)
    in nub firsts

--probabilityOf :: Eq a => Dist a -> (a -> Bool) -> Rational
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

--currently raises an exception for non-exhaustive patterns in lambda
--transform :: (a -> Dist b) -> Dist a -> Dist b
transform f xs = weightedDist (concat (map (\([(x, y)], z) -> [(x, y*z)]) (map (\ (a,b) -> (toList (f a), b)) (toList xs))))

--combine :: Dist a -> Dist b -> Dist (a, b)
combine xs ys = weightedDist (concat (map (\ x -> map (\ y -> ((fst x, fst y), snd x * snd y)) (toList ys)) (toList xs)))

--duplicate :: Dist a -> Int -> Dist [a]
duplicate num xs = if (num == 1)
                    then weightedDist (map (\(a,b) -> ([a], b)) (toList xs))
                    else weightedDist (map (\((a,b),c) -> (a:b,c)) (toList (combine xs (duplicate (num-1) xs))))


--
-- Private functions
--
totalProb :: [(a,Rational)] -> Rational
totalProb xs = foldl (\acc (a,b) -> acc + b) 0.0 xs

--A
solutionAd6 :: Dist Int
solutionAd6 = uniformDist[1..6]

solutionAd12 :: Dist Int
solutionAd12 = uniformDist[1..12]

--B
solutionB :: Rational
solutionB = 
    let d6 = uniformDist[1..6]
        d12= uniformDist[1..12]
    in probabilityOf (fmap (\(x, y) -> (x+y)) (combine d6 d12)) (== 11)

--C
solutionC1 :: Dist Int -> Rational
solutionC1 xs =
    let d = combine xs xs
        equal = (\(a,b) -> (((a,b) == (12, 6)) || ((a,b) == (6, 12)))) 
    in probabilityOf d equal

solutionC2 :: Dist Int-> Rational
solutionC2 xs =
    let ds = combine xs xs
        oned20 = (\(a, b) -> ((a == 12 && b /= 12) || (b == 12 && a /= 12)))
    in probabilityOf ds oned20
--D
solutionD :: Dist Int -> Rational
solutionD xs = (solutionC1 xs) * solutionB

--E
--F

--solutions F - I compile but raise exceptions from their calls to transform
solutionF :: Rational
solutionF =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
        enlist = fmap (\a -> [a]) coin
        joint = transform (\a -> duplicate a coin) d6
        threecount = (\a -> foldl (\acc b -> if (b == 'H')
                                            then (acc + 1)
                                            else (acc)) 0 a)
    in probabilityOf (fmap threecount joint) (\a -> (a == 3))
--G
solutionG :: Rational
solutionG =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
        enlist = fmap (\a -> [a]) coin
        joint = transform (\a -> duplicate a coin) d6
        threecount = (\a -> foldl (\acc b -> if (b == 'H')
                                            then (acc + 1)
                                            else (acc)) 0 a)
    in probabilityOf (fmap threecount joint) (\a -> (a >= 3))
--H
--I
solutionI :: Rational
solutionI =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
        joint = transform (\a -> duplicate a coin) d6
        threecount = (\a -> ((foldl (\acc b -> if (b == 'H')
                                            then (acc + 1)
                                            else (acc)) 0 a), length a))
        justthree = distFil (\(a,b) -> a == 3) (fmap threecount joint)
    in probabilityOf justthree (\(a,b) -> b == 3)

--J
--K
--L
--M
--N
{-factorial :: Int -> Int
factorial n = if n < 2 then 1 else n * factorial (n-1)

choose :: Int -> Int -> Rational
choose n k = (fromIntegral(factorial n)) / (fromIntegral((factorial k) * (factorial (n - k))))

binomialDist :: Int -> Int -> Rational -> Rational
binomialDist tries k p = (choose tries k)*(p^^(fromIntegral k))*((1.0-p)^^(fromIntegral(tries-k)))

solutionN :: [(Int, Rational)] -> Rational
solutionN xs =
    let d12 = uniformDist [1..12]
        gt16 = (totalProb (filter (\(x,y) -> x > 16) (map (\((a,b),c) -> (a+b, c)) (combine d12 d12))))
    in (probabilityOf (combine xs xs) (12,12)) * (binomialDist 30 3 gt16)
--O
--P
--Q
--R
--S
--T
--U
-}
--Look up:
--module definition