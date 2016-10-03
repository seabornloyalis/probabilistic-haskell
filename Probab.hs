--Probab.hs
--Author: Chad Myles
--Date: 9/26/16

import Data.List

data Dist a = Dist [(a,Rational)]
    deriving( Show)

instance Functor Dist where
    --fmap :: (a->b) -> Dist a -> Dist b
    fmap f (Dist xs) = Dist (map (\(x,p) -> (f x, p)) xs)

--dist :: [(a, Rational)] -> Dist a
--dist xs = xs
--TODO: make sure sum of Rationals = 1 and none are negative

unit :: a -> Dist a
unit x = Dist [(x,1.0)]

uniformDist :: [a] -> Dist a
uniformDist xs = Dist (map (\ x -> (x, (1.0 / len))) xs)
    where len = fromIntegral (length xs)

{-uniformDist :: [a] -> [(a, Rational)]
uniformDist xs = map (\ x -> (x, (1.0 / len))) xs
    where len = fromIntegral (length xs)-}

adjoin :: (a -> Dist b) -> Dist a -> Dist (a,b)
adjoin f (Dist xs) =
            let wrapped = map (\(k, p) ->
                            let (Dist res) = f k
                            in map (\(k',p') -> ((k,k'), p*p'))
                                res)
                                    xs
            in Dist (concat wrapped)

{-combine :: [(a, Rational)] -> [(b, Rational)] -> [((a, b), Rational)]
combine xs ys = concat (map (\ x -> map (\ y -> ((fst x, fst y), snd x * snd y)) ys) xs)-}

{-duplicateDist :: Integral a => a -> Dist b -> Dist [b]
duplicateDist num xs
    | num <= 0      =error "Can only call duplicateDist with positive integers"
    | num == 1      = map (\a -> [a]) xs
    | otherwise     = adjoin (\a' -> xs) (duplicateDist (num-1) xs)
-}
{-duplicateDist :: Integral a => a -> [(b, Rational)] -> [([b], Rational)]
duplicateDist num xs = duplicateDistHelper num xs

duplicateDistHelper :: Integral a => a -> [(b, Rational)] -> [([b], Rational)]
duplicateDistHelper num xs = if (num == 1)
                            then map (\(a,b) -> ([a], b)) xs
                            else map (\((a,b),c) -> (a:b,c)) (combine xs (duplicateDistHelper (num-1) xs))
-}
{-mapDist :: (a -> b) -> Dist a -> Dist b
mapDist f xs = map (\ (x,y) -> (f x, y)) xs

transform :: (a -> [(b, Rational)]) -> [(a, Rational)] -> [(b, Rational)]
transform f xs = concat (map (\([(x, y)], z) -> [(x, y*z)]) (map (\ (a,b) -> (f a, b)) xs))

distFil :: (a -> Bool) -> [(a, Rational)] -> [(a, Rational)]
distFil f xs =
    let intermed = filter (\ x -> f (fst x)) xs
    in map (\ (x,y) -> (x, y * (1.0 / totalProb(intermed)))) intermed


mergeEqual :: Eq a => [(a, Rational)] -> [(a, Rational)]
mergeEqual xs =
    let distinct = possibilites xs
    in map (\d -> (d, foldl (\acc (x,y) -> if (x == d)
                                           then (acc + y)
                                           else (acc))
                            0.0 xs)) distinct


deDist :: [(a, Rational)] -> [a]
deDist xs = map (\(x, y) -> x) xs

possibilites :: Eq a => [(a, Rational)] -> [a]
possibilites xs = nub (deDist xs)

probabilityOf :: Eq a => [(a, Rational)] -> a -> Rational
probabilityOf xs x = 
    let mergedDist = mergeEqual xs
    in if (x `elem` (deDist mergedDist))
        then snd (head (filter (\(f,s) -> f == x) (mergedDist)))
        else 0.0  

deriveDists :: Integral a => [(a, Rational)] -> [(b, Rational)] -> [([b], Rational)]
deriveDists nums xs = concat (map (\([(x,y)],z) -> [(x,y*z)]) (map (\(a, b) -> ((duplicateDist a xs), b)) nums))

totalProb :: [(a,Rational)] -> Rational
totalProb = sanityCheck

sanityCheck :: [(a,Rational)] -> Rational
sanityCheck xs = foldl (\acc (a,b) -> acc + b) 0.0 xs
-}
--A
{-solutionAd6 :: [(Int, Rational)]
solutionAd6 = uniformDist[1..6]

solutionAd12 :: [(Int, Rational)]
solutionAd12 = uniformDist[1..12]

--B
solutionB :: Rational
solutionB = 
    let d6 = uniformDist[1..6]
        d12= uniformDist[1..12]
    in probabilityOf (map (\((x, y), z) -> ((x+y), z)) (combine d6 d12)) 11

--C
solutionC1 :: [(Int, Rational)] -> Rational
solutionC1 xs =
    let d = combine xs xs
    in (probabilityOf d (6,12)) + (probabilityOf d (12,6))

solutionC2 :: [(Int, Rational)] -> Rational
solutionC2 xs =
    let ds = combine xs xs
    in (probabilityOf (collapse (\(a,b) -> b) ds) 12) -
        (probabilityOf (collapse (\(a,b) -> a) ds) 12) - (probabilityOf ds (12,12))
--D
solutionD :: [(Int, Rational)] -> Rational
solutionD xs = (solutionC1 xs) * solutionB

--E
--F
solutionF :: Rational
solutionF =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
        joint = deriveDists d6 coin
    in probabilityOf (map (\(a, b) -> (foldl (\acc x -> if x == 'H' then acc + 1 else acc) 0 a, b)) joint) 3

--G
solutionG :: Rational
solutionG =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
    in totalProb (filter (\(a, b) -> a >= 3) (map (\(a, b) -> (foldl (\acc x -> if x == 'H' then acc + 1 else acc) 0 a, b)) (deriveDists d6 coin)))

--H
--I
--solutionI :: Rational
--solutionI =
--    let d6 = uniformDist [1..6]
--        coin = uniformDist "HT"
--        joint = deriveDists d6 coin
--    in totalProb (filter (\((i, j), k) -> (length j) == 3) (distFil (\((a, b), c) -> a == 3) (map (\(a, b) -> (((foldl (\acc x -> if x == 'H' then acc + 1 else acc) 0 a), a), b)) joint)))
--ambiguous types for Eq

--J
--K
--L
--M
--N
factorial :: Int -> Int
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
diceIntToDist :: Int -> [(Int, Rational)]
diceIntToDist x
        | x == 4    = uniformDist [1..4]
        | x == 6    = uniformDist [1..6]
        | x == 8    = uniformDist [1..8]
        | x == 10   = uniformDist [1..10]
        | x == 12   = uniformDist [1..12]
        | otherwise = uniformDist [1..20]

--assuming with replacement
--solutionP :: [(Int, Rational)] -> Rational
--solutionP xs =
--    let dice = transform diceIntToDist xs
--    in 30.0 * totalProb (filter (\(a,b) -> a > 16) (map (\((a, b), c) -> ((a+b), c)) (combine dice dice)))
--generates exception non-exhaustive patterns in lambda
-}
--Look up:
--how to define new types in Haskell
--module definition