
module Tool.DistCat where

import Data.List (intersperse,subsequences,tails,maximumBy)
import Data.Function (on)

type Formatter = (String -> String -> Int -> String)
type Metric = String -> String -> Int

-- found at: http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring
lcstr :: Eq a => [a] -> [a] -> [a]
lcstr xs ys = maximumBy (compare `on` length) . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x, y) = if x == y then z ++ [x] else []

longestSubstringSimilarity :: Eq a => [a] -> [a] -> Int
longestSubstringSimilarity x y = length $ lcstr x y

-- found at: http://www.haskell.org/haskellwiki/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers)
	  lowers = eachDiag b a (mainDiag : lowers)
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z

printDist ::  Metric -> Formatter -> String -> String
printDist m f s = unlines . map (\(x,y,d) -> f x y d) $ generateDist m s

generateDist :: Metric -> String -> [(String,String,Int)]
generateDist f s = map (\[x,y] -> (x,y,f x y)) pairs
		where pairs = concatMap allPairs $ lines s
			where allPairs x = distinctPairs 2 $ words x

defaultFormat :: Formatter
defaultFormat x y d = concat $ intersperse " " [x,y,show d] 

distinctPairs :: Int -> [a] -> [[a]]
distinctPairs n xs = filter ((==n) . length) $ subsequences xs
