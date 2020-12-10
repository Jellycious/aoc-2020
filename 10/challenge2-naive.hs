import Data.List

-- Finds all possible arrangements using list monads
-- Too slow for big input

solve :: [Int] -> Int
solve ls = length $ solver ((0:ls)++[end]) (0,0) end
    where end = (maximum ls) + 3

possible :: [Int] -> (Int, Int) -> [(Int, Int)]
possible xs (i, prev) = valid
    where candidates    = zip ([(i+1)..(i+4)]) $ take 3 $ drop (i+1) xs
          valid         = filter (\(i, x) -> (x-prev)<=3) candidates

solver :: [Int] -> (Int, Int) -> Int -> [(Int, Int)]
solver ls (i,x) endclause = do
    (i',x') <- possible ls (i,x)
    if (x'==endclause) then return (i',x') else solver ls (i',x') endclause

main = do
    input <- lines <$> readFile "input.txt"
    nums <- return $ sort $ map read input :: IO [Int] 
    ans <- return $ solve nums
    putStrLn (show ans)
