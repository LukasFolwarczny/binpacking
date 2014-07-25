{-
 - Demo demonstrating proper usage of BinPacking module.
 - 2014 Lukáš Folwarczný <lfolwarczny@gmail.com>
 -}
import BinPacking
import Text.Printf

main = do
	example1
	example2
	example3
	example4
	example5
	example6
	example7

example1 = do
	putStrLn "---- Example #1 ----\n"
	putStrLn "Observe that Best Fit is better than First Fit here."
	putStr "Input sequence: "
	print $ seq
	displayBestFit seq
	displayFirstFit seq
	displayOptimalBP seq
	putStrLn ""
	where
	  seq = [0.3, 0.8, 0.2, 0.7]

example2 = do
	putStrLn "---- Example #2 ----\n"
	putStrLn "Observe that First Fit is better than Best Fit here."
	putStr "Input sequence: "
	print $ smallconv seq
	displayBestFit seq
	displayFirstFit seq
	displayOptimalBP seq
	putStrLn ""
	where
      seq = [0.6, 0.7, 0.1, 0.3, 0.3]

example3 = do
	putStrLn "---- Example #3 ----\n"
	putStrLn "See that dynamic programming is effective for large number of items with the same size."
	putStr "Input sequence (second element is number of items): "
	print $ map f seq
	displayFirstFit $ convertMU seq
	displayDynamicBP seq
	putStrLn ""
	where
	  seq = [(0.33,8), (0.31,10), (0.66,8), (0.67,10)]
	  f (a, b) = (fromRational a, b)

example4 = do
	putStrLn "---- Example #4 ----\n"
	putStrLn "This example shows that First Fit Decreasing is (even asymptotically!) no better than 11/9 approximation."
	putStr "Short input sequence: "
	print $ smallconv seq
	displayFirstFitDecreasing seq
	displayDynamicBP $ convertUM seq
	putStr "Double input sequence (optimal solution is 18 and may be computed with dynamic solution, however not instantly): "
	print $ smallconv seq2
	displayFirstFitDecreasing seq2
	putStr "Double double input sequence: "
	print $ smallconv seq3
	displayFirstFitDecreasing seq3
	putStrLn ""
	where seq = (concat $ replicate 6 [1/4 - 0.02, 1/4 + 0.01, 1/2 + 0.01]) ++ (concat $ replicate 3 [1/4 - 0.02, 1/4 - 0.02, 1/4 + 0.02, 1/4 + 0.02])
	      seq2 = seq ++ seq
	      seq3 = seq2 ++ seq2

example5 = do
	putStrLn "---- Example #5 ----\n"
	putStrLn "APTAS returns solution with at most (1+epsilon)*OPT + 1 bins."
	putStr "Input sequence: "
	print $ smallconv seq
	displayAPTAS 0.5 seq
	displayAPTAS 0.1 seq
	displayOptimalBP seq
	putStrLn ""
	where seq = [0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.53, 0.54, 0.55, 0.56, 0.57, 0.58]

example6 = do
	putStrLn "---- Example #6 ----\n"
	putStrLn "See that unbounded Best Fit is here better than two-bounded-space optimum."
	putStr "Input sequence: "
	print $ smallconv seq
	displayBestFit seq
	displayBestFitBounded 2 seq
	displayOptimalBPBounded 2 seq
	putStrLn ""
	where seq = [0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2]

example7 = do
	putStrLn "---- Example #7 ----\n"
	putStrLn "This example ilustrates how to show that two-bounded-space Best Fit is no better than 3/2-approximation."
	putStr "\nInput sequence 1: "
	putStrLn $ transf seq1
	printf "Optimal 2-bounded solution: %d bins used\n" (length osol1)
	putStrLn $ transfSol osol1
	printf "Best Fit 2-bounded: %d bins used\n" (length sol1)
	putStrLn $ transfSolO sol1
	putStr "\nInput sequence 2: "
	putStrLn $ transf seq2
	printf "Optimal 2-bounded solution: %d bins used\n" (length osol2)
	putStrLn $ transfSol osol2
	printf "Best Fit 2-bounded: %d bins used\n" (length sol2)
	putStrLn $ transfSolO sol2
	putStr "\nInput sequence 3: "
	putStrLn $ transf seq3
	printf "Optimal 2-bounded solution: %d bins used\n" (length osol3)
	putStrLn $ transfSol osol3
	printf "Best Fit 2-bounded: %d bins used\n" (length sol3)
	putStrLn $ transfSolO sol3
	putStrLn ""
	where
	  seq1 = [1/3, 1/3, 2/3, 2/3]
	  sol1 = bestFitBounded 2 seq1
	  osol1 = optimalBPBounded 2 seq1
	  seq2 = [1/3, 1/3, 2/3, 2/3, 1/3 + 0.01, 1/3 + 0.01, 2/3 - 0.01, 2/3 - 0.01]
	  sol2 = bestFitBounded 2 seq2
	  osol2 = optimalBPBounded 2 seq2
	  seq3 = [1/3, 1/3, 2/3, 2/3, 1/3 + 0.01, 1/3 + 0.01, 2/3 - 0.01, 2/3 - 0.01, 1/3 + 0.02, 1/3 + 0.02, 2/3 - 0.02, 2/3 - 0.02]
	  sol3 = bestFitBounded 2 seq3
	  osol3 = optimalBPBounded 2 seq3
	  transf = filter (/= '"') . show . map converter
	  transfSol = filter (/= '"') . show . (map $ map converter)
	  transfSolO = filter (/= '"') . show . (map $ map f)
	  f (a,b) = (converter a, b)

converter :: Rational-> String
converter x
  | x == 1/3 = "1/3"
  | x == 2/3 = "2/3"
  | abs (x - 1/3) < 0.1 && x < 1/3 = "1/3 - " ++ f (x - 1/3)
  | abs (x - 1/3) < 0.1 && x > 1/3 = "1/3 + " ++ f (x - 1/3)
  | abs (x - 2/3) < 0.1 && x < 2/3 = "2/3 - " ++ f (x - 2/3)
  | abs (x - 2/3) < 0.1 && x > 2/3 = "2/3 + " ++ f (x - 2/3)
  where f x = show $ (fromIntegral $ round (1000 * (abs x)) ) / 1000.0
	  
displayAlg :: String -> (t -> [a]) -> ([a] -> s) ->  -> IO ()
displayAlg name alg conv xs = do
	printf "%s: %d bins used\n" name (length result)
	print $ conv result
	where result = alg xs

displayBestFit = displayAlg "Best Fit" bestFit convO
displayFirstFit = displayAlg "First Fit" firstFit convO
displayFirstFitDecreasing = displayAlg "First Fit Decreasing" firstFitDecreasing convU
displayOptimalBP = displayAlg "Optimal solution" optimalBP convU
displayDynamicBP = displayAlg "Optimal solution with dynamic programming" dynamicBP convO
displayAPTAS eps = displayAlg ("APTAS with epsilon = " ++ (show $ fromRational eps)) (aptasBP eps) convU
displayBestFitBounded k = displayAlg ("Best Fit " ++ (show k) ++ "-bounded") (bestFitBounded k) convO
displayOptimalBPBounded k = displayAlg ("Optimal " ++ (show k) ++ "-bounded solution") (optimalBPBounded k) convU

convO :: [Bin] -> [[(Double,Int)]]
convO = map $ map f
 where f (a, b) = (fromRational a, b)

convU :: [UBin] -> [[Double]]
convU xs = map (map fromRational) xs

convM :: [Bin] -> [[(Double,Int)]]
convM = map $ map f
 where f (a, b) = (fromRational a, b)

smallconv :: UBin -> [Double]
smallconv = map fromRational
