--ProbabFP.hs
--Author: Chad Myles
--Date: 9/26/16

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

deDist :: Dist a -> [(a, Float)]

mergeEqual :: Eq a => Dist a -> Dist a

possibilities :: Eq a => Dist a -> [a]

probabilityOf :: Eq a => Dist a -> a -> Float

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

--deDist :: Dist a -> [(a, Float)]
deDist (Dist valList ) = valList

--mergeEqual :: Eq a => [(a, Float)] -> [(a, Float)]
mergeEqual xs =
    let distinct = possibilities xs
    in weightedDist (map (\y -> (y, (foldl (\acc (b,c) ->if (b == y)
                                                        then (acc + c)
                                                        else acc) 0.0 (deDist xs)))) distinct)

--possibilites :: Eq a => Dist a -> [a]
possibilities xs =
    let firsts = map (\(a,b) -> a) (deDist xs)
    in nub firsts

--probabilityOf :: Eq a => [(a, Float)] -> a -> Float
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

--duplicate :: Dist a -> Int -> Dist [a]
duplicate num xs = if (num == 1)
                    then weightedDist (map (\(a,b) -> ([a], b)) (toList xs))
                    else weightedDist (map (\((a,b),c) -> (a:b,c)) (toList (combine xs (duplicate (num-1) xs))))


--
-- Private functions
--
totalProb :: [(a,Float)] -> Float
totalProb xs = foldl (\acc (a,b) -> acc + b) 0.0 xs

