--Probab.hs
--Author: Chad Myles
--Date: 9/26/16

import Data.List

--uncomment the below line for quicker, floating point arithmetic
--data Dist a = Dist [(a, Float)]

--leave the below line uncommented for exact floating point arithmetic
--(at some performance cost)
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

--deDist :: Dist a -> [(a, Float)]
deDist :: Dist a -> [(a, Rational)]

mergeEqual :: Eq a => Dist a -> Dist a

possibilities :: Eq a => Dist a -> [a]

--probabilityOf :: Eq a => Dist a -> a -> Float
probabilityOf :: Eq a => Dist a -> a -> Rational

adjoin :: (a -> Dist b) -> Dist a -> Dist (a,b)

distFil :: (a -> Bool) -> Dist a -> Dist a

transform :: (a -> Dist b) -> Dist a-> Dist b

combine :: Dist a -> Dist b -> Dist (a, b)
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

--deDist :: Dist a -> [(a, Rational)]
deDist (Dist valList ) = valList

--mergeEqual :: Eq a => [(a, Rational)] -> [(a, Rational)]
mergeEqual xs =
    let distinct = possibilities xs
    in weightedDist (map (\y -> (y, (foldl (\acc (b,c) ->if (b == y)
                                                        then (acc + c)
                                                        else acc) 0.0 (deDist xs)))) distinct)

--possibilites :: Eq a => Dist a -> [a]
possibilities xs =
    let firsts = map (\(a,b) -> a) (deDist xs)
    in nub firsts

--probabilityOf :: Eq a => [(a, Rational)] -> a -> Rational
probabilityOf xs x = 
    let mergedDist = mergeEqual xs
    in if (x `elem` (possibilities mergedDist))
        then snd (head (filter (\(f,s) -> f == x) (deDist mergedDist)))
        else 0.0  

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
    let intermed = filter (\ x -> f (fst x)) (deDist xs)
    in weightedDist (map (\ (x,y) -> (x, y * (1.0 / totalProb(intermed)))) intermed)

--transform :: (a -> Dist b) -> Dist a-> Dist b
transform f xs = weightedDist (concat (map (\([(x, y)], z) -> [(x, y*z)]) (map (\ (a,b) -> (deDist (f a), b)) (deDist xs))))

--combine :: Dist a -> Dist b -> Dist (a, b)
combine xs ys = weightedDist (concat (map (\ x -> map (\ y -> ((fst x, fst y), snd x * snd y)) (deDist ys)) (deDist xs)))

--deriveDists :: Integral a => Dist a -> Dist b -> Dist [b]
--deriveDists nums xs = concat (map (\([(x,y)],z) -> [(x,y*z)]) (map (\(a, b) -> ((duplicateDist a xs), b)) nums))
--
-- Private functions
--
totalProb :: [(a,Rational)] -> Rational
totalProb xs = foldl (\acc (a,b) -> acc + b) 0.0 xs

{-duplicateDist :: Integral a => a -> [(b, Rational)] -> [([b], Rational)]
duplicateDist num xs = duplicateDistHelper num xs

duplicateDistHelper :: Integral a => a -> [(b, Rational)] -> [([b], Rational)]
duplicateDistHelper num xs = if (num == 1)
                            then map (\(a,b) -> ([a], b)) xs
                            else map (\((a,b),c) -> (a:b,c)) (combine xs (duplicateDistHelper (num-1) xs))

deriveDists :: Integral a => [(a, Rational)] -> [(b, Rational)] -> [([b], Rational)]
deriveDists nums xs = concat (map (\([(x,y)],z) -> [(x,y*z)]) (map (\(a, b) -> ((duplicateDist a xs), b)) nums))
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