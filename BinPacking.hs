{-|
Module      : BinPacking
Description : Algorithms for the Bin Packing problem
Copyright   : Lukáš Folwarczný, 2014
Maintainer  : lfolwarczny@gmail.com
-}
module BinPacking
(
-- * Data types
SizeT, Item, UItem, MItem, Bin, UBin, MBin,
-- * Online algorithms
firstFit,
bestFit,
-- * Offline algorithms
firstFitDecreasing,
optimalBP,
dynamicBP,
aptasBP,
-- * Bounded-Space Bin Packing
bestFitBounded,
optimalBPBounded,
-- * General versions
firstFit',
bestFit',
bbf',
-- * Utilities
convertUO, convertOU, convertMU, convertUM)

where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M

-- Online algorithms (Best Fit, First Fit) use ordered items since the order in which
-- items are processed is important.
-- Offline algorithms use unordered items, dynamic programming uses multi items since it
-- can process them effectively.

type SizeT = Rational
type Item = (SizeT, Int)  -- Ordered item (size, index)
type UItem = SizeT        -- Unordered item, size only
type MItem = (SizeT, Int) -- Multi item (size, count)
type Bin = [Item]
type UBin = [UItem]
type MBin = [MItem]

binSize :: Bin -> SizeT
binSize = sum . map fst
binSizes :: [Bin] -> [SizeT]
binSizes = map binSize

convertUO :: [UItem] -> [Item]
convertUO = map (\x -> (x, 0))

convertOU :: [Item] -> [UItem]
convertOU = map fst

convertMU :: [MItem] -> [UItem]
convertMU = concatMap f
  where f (size, n) = replicate n size

convertUM :: [UItem] -> [MItem]
convertUM = map (\x -> (head x, length x)) . group . sort

stdFitsIntoBin s bin = sum (s:bin) <= 1.0

firstFit' :: (a -> [a] -> Bool) -> [a] -> [[a]]
firstFit' fitsIntoBin = foldl (firstFitAddItem' fitsIntoBin) []

firstFitAddItem' :: (a -> [a] -> Bool) -> [[a]] -> a -> [[a]]
firstFitAddItem' fitsIntoBin bins item
  | isNothing $ find (fitsIntoBin item) bins = bins ++ [[item]]
  | otherwise = updateFirst (fitsIntoBin item) (++[item]) bins

-- | The /First Fit/ algorithm. Each item is put into the first bin where it fits.
--
-- > firstFit [0.3, 0.8, 0.2, 0.7] == [ [0.3, 0.2], [0.8], [0.7] ]
-- > firstFit [0.6, 0.7, 0.1, 0.3, 0.3] == [ [0.6, 0.1, 0.3], [0.7, 0.3] ]
firstFit :: [UItem] -> [UBin]
firstFit = firstFit' fitsIntoBin
  where fitsIntoBin it bin = sum (it:bin) <= 1.0

-- TODO: This is obsolete but used in APTAS.
firstFitAddItem :: Item -> [Bin] -> [Bin]
firstFitAddItem it@(s,_) bins
  | find fitsIntoBin bins == Nothing = bins ++ [[it]]
  | otherwise = updateFirst fitsIntoBin (++[it]) bins
  where fitsIntoBin = (<= 1-s) . binSize

-- | 'firstFitDecreasing' is the same as 'firstFit', but items are sorted in
-- decreasing order before being processed by the First Fit algorithm.
--
-- > firstFitDecreasing [0.3, 0.8, 0.2, 0.7] == [ [0.8, 0.2], [0.7, 0.3] ]
firstFitDecreasing :: [UItem] -> [UBin]
firstFitDecreasing = firstFit . reverse . sort

bestFit' :: Ord b => (a -> [a] -> Bool) -> ([a] -> b) -> [a] -> [[a]]
bestFit' fitsIntoBin binSize = foldl (bestFitAddItem' fitsIntoBin binSize) []

bestFitAddItem' :: Ord b => (a -> [a] -> Bool) -> ([a] -> b) -> [[a]] -> a -> [[a]]
bestFitAddItem' fitsIntoBin binSize bins item
  | null goodBins = bins ++ [[item]]
  | otherwise = updateFirst ((== optSize) . binSize) (++[item]) bins
  where goodBins = filter (fitsIntoBin item) bins
        optSize = maximum $ map binSize goodBins

-- | The /Best Fit/ algorithm. Each item is put into the fullest bin where it fits.
--
-- > bestFit [0.3, 0.8, 0.2, 0.7] == [ [0.3, 0.7], [0.8, 0.2] ]
-- > bestFit [0.6, 0.7, 0.1, 0.3, 0.3] == [ [0.6, 0.3], [0.7, 0.1], [0.3] ]
bestFit :: [UItem] -> [UBin]
bestFit = bestFit' fitsIntoBin sum
  where fitsIntoBin it bin = sum (it:bin) <= 1.0



-- Bounded Best Fit
bbf' :: Ord b => Int -> (a -> [a] -> Bool) -> ([a] -> b) -> [a] -> [[a]]
bbf' k fitsIntoBin binSize = uncurry (++) . foldl (bbfAddItem' fitsIntoBin binSize) ([], take k $ repeat [])

-- Input and output have both exactly k open bins.
bbfAddItem' :: Ord b => (a -> [a] -> Bool) -> ([a] -> b) -> ([[a]], [[a]]) -> a -> ([[a]], [[a]])
bbfAddItem' fitsIntoBin binSize (closed, open) item
  | null goodBins = (closed ++ [fullest], open' ++ [[item]])
  | otherwise = (closed, updateFirst ((== bestSize) . binSize) (++[item]) open)
  where goodBins = filter (fitsIntoBin item) open
        (open', fullest) = extractMaximum (comparing binSize) open
        bestSize = maximum $ map binSize $ filter (fitsIntoBin item) open

-- | 'bestFitBounded' k is the /Best Fit/ algorithm for the /k/-bounded-space variant of
-- Bin Packing.
--
-- > bestFitBounded 2 [0.5, 0.6, 0.7, 0.3, 0.4, 0.5] == [ [0.6], [0.7, 0.3], [0.5, 0.4], [0.5] ]
-- > bestFitBounded 3 [0.5, 0.6, 0.7, 0.3, 0.4, 0.5] == [ [0.5, 0.5], [0.6, 0.4], [0.7, 0.3] ]
bestFitBounded :: Int -> [UItem] -> [UBin]
bestFitBounded k = bbf' k fitsIntoBin sum
  where fitsIntoBin it bin = sum (it:bin) <= 1.0


---- Optimal solution (brute force) ----

optimalBP :: [UItem] -> [UBin]
optimalBP = optimal []

optimalBPPerf :: [UItem] -> Int
optimalBPPerf = length . optimalBP

optimal :: [UBin] -> [UItem] -> [UBin]
optimal bins [] = bins
optimal bins (x:xs) = minimumBy (comparing length) [ optimal b xs | b <- genStates bins x ]

genStates :: [UBin] -> UItem -> [[UBin]]
genStates bins it = (bins ++ [[it]]):(multiApplyIf (++[it]) ((<= 1-it) . sum) bins)

---- Optimal solution for bounded case ----

optimalBPBounded :: Int -> [UItem] -> [UBin]
optimalBPBounded k = filter (not . null) . optimalBounded (replicate k [], [])

optimalBPBoundedPerf :: Int -> [UItem] -> Int
optimalBPBoundedPerf k = length . optimalBPBounded k

optimalBounded :: ([UBin], [UBin]) -> [UItem] -> [UBin]
optimalBounded bins [] = uncurry (++) bins
optimalBounded bins@(open, _) (x:xs) =
  minimumBy (comparing length) [ optimalBounded (optimalPutIntoBin x i bins) xs | i <- [0..(length open)-1] ]

optimalPutIntoBin :: UItem -> Int -> ([UBin], [UBin]) -> ([UBin], [UBin])
optimalPutIntoBin item i (open, closed)
  | sum (open !! i) + item <= 1 = (applyAt i (++[item]) open, closed)
  | otherwise = (applyAt i (\x -> [item]) open, closed ++ [open !! i])

---- Dynamic programming (optimal solution) ----

dynamicBP :: [MItem] -> [MBin]
dynamicBP items = map (filter $ (/= 0) . snd) $ snd $ mymap M.! items
  where
  binConfigs = filter ((/= 0) . sum . map totalSize) $ binConfigurations items
  mymap = M.fromList $ [ (st, f st) | st <- dynamicStates items ]
  f ys
    | (sum $ map totalSize ys) == 0 = (0, [])
    | otherwise = minimumBy (comparing fst)
	  [ (1 + n, zs:packing)  | zs <- binConfigs, let newState = subMLists ys zs, validMList $ newState, let (n, packing) = mymap M.! newState ]

-- dynamicBPPerf is a bit faster than dynamicBP because it stores less data
dynamicBPPerf :: [MItem] -> Int
dynamicBPPerf items = mymap M.! items
  where
  binConfigs = filter ((/= 0) . sum . map totalSize) $ binConfigurations items
  mymap = M.fromList $ [ (st, f st) | st <- dynamicStates items ]
  f ys
    | (sum $ map totalSize ys) == 0 = 0
    | otherwise = 1 + minimum [ mymap M.! newState | zs <- binConfigs, let newState = subMLists ys zs, validMList $ newState ]

-- Generate all valid options how to put items into a single bin
binConfigurations :: [MItem] -> [[MItem]]
binConfigurations [] = [[]]
binConfigurations ((size,count):xs) = [ newConf | conf <- binConfigurations xs, c <- [0..count], let newConf = (size, c):conf, fulfilsCapacity newConf]
  where fulfilsCapacity = (<= 1) . sum . map totalSize

dynamicStates :: [MItem] -> [[MItem]]
dynamicStates [] = [[]]
dynamicStates ((size,count):xs) = [ (size,c) : state | state <- dynamicStates xs, c <- [0..count] ]

totalSize :: MItem -> SizeT
totalSize (size,count) = size * (fromIntegral count)

-- Subtraction on lists of multi items
subMLists :: [MItem] -> [MItem] -> [MItem]
subMLists = zipWith pairSubtraction
  where pairSubtraction (s, n) (t, m) | s == t = (s, n - m)

-- Test if all counts are non-negative
validMList :: [MItem] -> Bool
validMList = all $ (>= 0) . snd


---- Asymptotic polynomial time approximation scheme (APTAS) ----
-- Packs the items into (1 + eps)*OPT + 1 bins. OPT is the number of bins the optimal
-- algorithm needs.

aptasBP :: SizeT -> [UItem] -> [UBin]
aptasBP eps seq = map convertOU $ foldl (flip firstFitAddItem) bigPacking small
  where
  gamma = eps/2
  (big, small) = partition ((>gamma) . fst) (zip seq [1..])
  bigPacking = aptasBig eps big

aptasBPPerf :: SizeT -> [UItem] -> Int
aptasBPPerf eps = length . aptasBP eps

-- Pack big items, all items given must be > eps/2
aptasBig :: SizeT -> [Item] -> [Bin]
aptasBig eps items = foldl (flip firstFitAddItem) roundedPacking fstGroup
  where
  k = max (floor(eps * (sum $ map fst items))) 1
  fstGroup:groups = groupList k $ reverse $ sortBy (comparing fst) items
  roundedPacking = aptasRounded groups

-- Rounds all items of each group to the size of largest item in the group,
-- calls dynamicBP and unrounds the item
aptasRounded :: [[Item]] -> [Bin]
aptasRounded groups = map convertUO unroundedPacking
  where
  roundGroup gs@((size,_):_) = (size, length gs)
  rounding = map h groups
  h gs@((size,_):_) = (size, map fst gs)
  dynamicPacking = dynamicBP $ map roundGroup groups
  unorderedDynamicPacking = map convertMU dynamicPacking
  unroundedPacking = snd $ mapAccumL unroundBin rounding unorderedDynamicPacking

unroundBin :: [(SizeT, [UItem])] -> UBin -> ([(SizeT, [UItem])], UBin)
unroundBin = mapAccumL unroundItem

unroundItem :: [(SizeT, [UItem])] -> UItem -> ([(SizeT, [UItem])], UItem)
unroundItem rounding rounded
  | null $ tail origSizes = (xs ++ ys, head origSizes)
  | otherwise = (xs ++ [(rounded, tail origSizes)] ++ ys, head origSizes)
  where (xs, (_, origSizes):ys) = span ((/= rounded) . fst) rounding

---- Utilities ----

updateFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateFirst pred f (x:xs)
  | pred x = (f x):xs
  | otherwise = (x:updateFirst pred f xs)

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst pred (x:xs)
  | pred x = xs
  | otherwise = (x:deleteFirst pred xs)

extractMaximum :: (a -> a -> Ordering) -> [a] -> ([a],a)
extractMaximum comparator xs = (as ++ bs, m)
  where maxelem = maximumBy comparator xs
        (as,m:bs) = span (\x -> comparator maxelem x /= EQ) xs

updateMaximum :: (a -> a -> Ordering) -> (a -> a) -> [a] -> [a]
updateMaximum comparator f xs = as ++ (f m:bs)
  where maxelem = maximumBy comparator xs
        (as,m:bs) = span (\x -> comparator maxelem x /= EQ) xs

multiApplyIf :: (a -> a) -> (a -> Bool) -> [a] -> [[a]]
multiApplyIf f pred xs = [ applyAt i f xs | i <- [0..(length xs)-1], pred (xs !! i) ]

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt 0 f (x:xs) = (f x):xs
applyAt n f (x:xs) = x:(applyAt (n-1) f xs)

groupList :: Int -> [a] -> [[a]]
groupList k [] = []
groupList k xs = (take k xs):(groupList k (drop k xs) )
