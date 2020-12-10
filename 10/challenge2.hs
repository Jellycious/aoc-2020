import Data.List
import qualified Data.Map as M
import Data.Maybe

-- Dynamic Programming Solution
possible :: [Int] -> Int  -> [Int]
possible ls x = filter (\x' -> (x'-x)<=3) cands
    where cands = take 3 ls

solver :: M.Map Int Int -> [Int] -> [Int] -> M.Map Int Int
solver m [] ls = m
solver m (x:xs) ls = solver m' xs (x:ls)
    where cands = possible ls x 
          arr   = sum $ map (\x -> fromJust $ M.lookup x m) cands
          m'    = M.insert x arr m

solve :: [Int] -> Int -> M.Map Int Int
solve nums end = solver (M.singleton end 1) (reverse (0:nums)) [end]

main :: IO Int
main = do
    input <- lines <$> readFile "input.txt"
    nums <- return $ sort $ map read input :: IO [Int] 
    sol <- return $ solve nums $ maximum nums + 3
    return $ sol M.! 0
    
