prev25 :: [Int] -> Int -> [Int]
prev25 ls index = take 25 $ drop (index - 25) ls

hasSum :: [Int] -> Int -> Bool
hasSum ls sum = (length $ take 1 [(x,y) | x <- ls, y <- ls, y+x==sum]) > 0

findWeakness :: [Int] -> [(Int, Int)] -> Int
findWeakness ls [] = error "Weakness not found"
findWeakness ls ((i, x):xs) = if hasSum prevs x then findWeakness ls xs else x
    where prevs = prev25 ls i

invalidNum nums = findWeakness nums (drop 25 $ zip [0..] nums)


main = do
    input <- lines <$> readFile "input.txt"
    nums <- pure $ map read input :: IO [Int] 
    answer <- pure $ invalidNum nums
    return answer

