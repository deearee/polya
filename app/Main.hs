module Main where

import Data.Bits
import Data.Char
import qualified Data.Vector as V
import Data.List (delete, sort)
import System.Environment (getArgs)

encodeGraph6 :: Int -> [(Int, Int)] -> String
encodeGraph6 n edges = encodeN n ++ encodeEdges n edges

encodeN :: Int -> String
encodeN n
  | n < 0     = error "Number of vertices must be non-negative"
  | n < 63    = [chr (n + 63)]
  | n < 258048 = chr 126 : encodeBase64 [n]
  | otherwise = chr 126 : chr 126 : encodeBase64 [n]

encodeBase64 :: [Int] -> String
encodeBase64 = map (chr . (+63) . (.&. 63))

encodeEdges :: Int -> [(Int, Int)] -> String
encodeEdges n edges = encodeBase64 packedBits
  where
    adjMatrix = V.accum (||) (V.replicate (n*n) False) 
                [(i*n+j, True) | (i,j) <- edges ++ map swap edges]
    swap (a,b) = (b,a)
    upperTriangle = [adjMatrix V.! (i*n + j) | i <- [0..n-2], j <- [i+1..n-1]]
    packedBits = pack 6 0 upperTriangle

pack :: Int -> Int -> [Bool] -> [Int]
pack 0 acc [] = [acc]
pack 0 acc xs = acc : pack 6 0 xs
pack k acc [] = pack (k-1) (acc * 2) []
pack k acc (x:xs) = pack (k-1) (acc * 2 + if x then 1 else 0) xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [zs | ys <- subsets xs, zs <- [ys, (x:ys)]]

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:ps | x <- xs, ps <- permutations (delete x xs) ]

degreeList :: Int -> [(Int, Int)] -> [Int]
degreeList n g =
    sort $ V.toList $ (V.accum (+) (V.replicate n 0) $ [(i, 1) | (i, _) <- g] ++ [(j, 1) | (_, j) <- g])

applyPermutation :: [(Int, Int)] -> [Int] -> [(Int, Int)]
applyPermutation g p = [if p !! i < p !! j then (p !! i, p !! j) else (p !! j, p !! i) | (i, j) <- g]

allGraphs :: Int -> [[(Int, Int)]]
allGraphs n = subsets [(i, j) | i <- [0..n-1], j <- [i+1..n-1]]

areIsomorphic :: Int -> [(Int, Int)] -> [(Int, Int)] -> Bool
areIsomorphic n g1 g2 = 
    (length g1) == (length g2)
    && (degreeList n g1) == (degreeList n g2)
    && (any ((g2 ==) . sort . applyPermutation g1) $ permutations [0..n-1])

nonIsomGraphs :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
nonIsomGraphs _ [] = []
nonIsomGraphs n (x:xs) = if any (areIsomorphic n x) xs then (nonIsomGraphs n xs) else x:(nonIsomGraphs n xs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "The first argument has to be the number of vertices"
        arg1:_ -> 
            let n = read arg1 in
            mapM_ putStrLn $ (map (encodeGraph6 n)) $ nonIsomGraphs n $ allGraphs n
