import Data.List

solve (y:[]) diffs = (3:diffs)
solve (x:y:ys) diffs = solve (y:ys) ((y-x):diffs)


main = do
    input <- lines <$> readFile "input.txt"
    nums <- return $ sort $ map read input :: IO [Int] 
    ans <- return $ solve (0:nums) []
    threes <- return $ (length . filter (==3)) ans
    ones <- return $ (length . filter (==1)) ans
    return (threes * ones)
