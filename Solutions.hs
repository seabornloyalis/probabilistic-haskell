module Main where

import System.Environment
import System.Exit
import Data.List
import Probab

main :: IO ()
main = do
    args <- getArgs
    if (null args)
        then putStrLn (concat (map (\a -> (showSolution a) ++ "\n")
            ["A", "B", "C", "D", "F", "G", "I", "J", "K"]))
        else putStrLn (concat (map (\a -> (showSolution a) ++ "\n") args))

showSolution :: String -> String
showSolution "A" =
    let p1 = solutionAd6
        p2 = solutionAd12
        a = "A.1. throwing a single d6 = " ++ (show p1) ++ "\n"
        b = "A.2. throwing a single d12 = " ++ (show p2) ++ "\n"
    in a ++ b
showSolution "B" =
    let b = solutionB
    in  "B. P(d6+d12) = " ++ (show (100 * b)) ++ "%\n"
showSolution "C" = 
    let params = weightedDist [(6,0.19565),(8,0.19565), (12,0.304347), (20,0.304347)]
        c1 = (solutionC1 params)
        c2 = (solutionC2 params)
        p1 = "C.1. P(draw d6 and d12) = " ++ (show (100 * c1)) ++"%\n"
        p2 = "C.1. P(exactly one d20) = " ++ (show (100 * c2)) ++"%\n"
    in  p1 ++ p2
showSolution "D" =
    let params = weightedDist [(6,0.19565),(8,0.19565), (12,0.304347), (20,0.304347)]
        d = solutionD params
    in "D. P(draw d6 and d12, throw 11) = " ++ (show (100*d)) ++ "%\n"
showSolution "F" = "F. Throw d6; toss that many coins; probability of 3 heads = "
                    ++ (show (100 * solutionF)) ++ "%\n"
showSolution "G" = "G. Throw d6; toss that many coins; probability of at least 3 heads = "
                    ++ (show (100*solutionG)) ++ "%\n"
showSolution "I" = "I. Throw d6; toss that many coins; see 3 heads; P(N=3) = "
                    ++ (show (100*solutionI)) ++ "%\n"
showSolution "J" = "J. Throw d6; toss that many coins; N > 4; P(heads=3) = "
                    ++ (show (100*solutionJ)) ++ "%\n"
showSolution "K" = "K. (identical to I) =" ++ (show (100*solutionK)) ++ "%\n"
showSolution "N" =
    let params = weightedDist [(6,0.19565),(8,0.19565), (12,0.304347), (20,0.304347)]
    in "N. P(draw d12+d12 and put 3 marks in right) = "
        ++ (show (100*(solutionN params))) ++ "%\n"
showSolution x = "Solution not implemented or argument invalid"

--A
solutionAd6 :: Dist Int
solutionAd6 = uniformDist[1..6]

solutionAd12 :: Dist Int
solutionAd12 = uniformDist[1..12]

--B
solutionB :: Float
solutionB = 
    let d6 = uniformDist[1..6]
        d12= uniformDist[1..12]
    in probabilityOf (fmap (\(x, y) -> (x+y)) (combine d6 d12)) (== 11)

--C
solutionC1 :: Dist Int -> Float
solutionC1 xs =
    let d = combine xs xs
        equal = (\(a,b) -> (((a,b) == (12, 6)) || ((a,b) == (6, 12)))) 
    in probabilityOf d equal

solutionC2 :: Dist Int-> Float
solutionC2 xs =
    let ds = combine xs xs
        oned20 = (\(a, b) -> ((a == 12 && b /= 12) || (b == 12 && a /= 12)))
    in probabilityOf ds oned20
--D
solutionD :: Dist Int -> Float
solutionD xs = (solutionC1 xs) * solutionB

--E
--F

solutionF :: Float
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
solutionG :: Float
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
solutionI :: Float
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
solutionJ :: Float
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
solutionK :: Float
solutionK = solutionI
--L
--M
--N
solutionN :: Dist Int -> Float
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


--O
--P
--Q
--R
--S
--T
--U