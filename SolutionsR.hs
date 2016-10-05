import System.Environment
import System.Exit
import Data.List

import Probab

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
solutionJ :: Rational
solutionJ =
    let d6 = uniformDist [1..6]
        coin = uniformDist "HT"
        intermed = distFil (\a -> a > 4) d6   
        joint = transform (\a -> duplicate a coin) intermed
        threecount = (\a -> ((foldl (\acc b -> if (b == 'H')
                                            then (acc + 1)
                                            else (acc)) 0 a)))
    in probabilityOf (fmap threecount joint) (\a -> a == 3)

--K
solutionK :: Rational
solutionK = solutionI

--L
--M
--N
solutionN :: Dist Int -> Rational
solutionN xs =
    let d12 = uniformDist [1..12]
        twodice = combine xs xs
        test = duplicate 30 (fmap (\(a,b) -> if ((a + b) > 16)
                                            then "R"
                                            else "LorM") (combine d12 d12))
        threeGT16 = probabilityOf test (\a -> (foldl (\acc b -> if (b == "R")
                                                            then (acc + 1)
                                                            else acc) 0 a) == 3)
    in (probabilityOf twodice (\(a, b) -> (a == 12) && (b == 12))) * threeGT16
    
{-
factorial :: Int -> Int
factorial n = if n < 2 then 1 else n * factorial (n-1)

choose :: Int -> Int -> Rational
choose n k = (fromIntegral(factorial n)) / (fromIntegral((factorial k) * (factorial (n - k))))

binomialDist :: Int -> Int -> Rational -> Rational
binomialDist tries k p = (choose tries k)*(p^^(fromIntegral k))*((1.0-p)^^(fromIntegral(tries-k)))

solutionNStats :: Dist Int -> Rational
solutionNStats xs =
    let d12 = uniformDist [1..12]
        gt16 = (probabilityOf (fmap (\(a,b) -> a+b) (combine d12 d12)) (\a -> a > 16))
        twodice = (combine xs xs)
    in (probabilityOf twodice (\(a, b) -> (a == 12) && (b == 12))) * (binomialDist 30 3 gt16)
-}
--O
--P
--Q
--R
--S
--T
--U