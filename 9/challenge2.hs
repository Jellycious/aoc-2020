prev25 :: [Int] -> Int -> [Int]
prev25 ls index = take 25 $ drop (index - 25) ls

hasSum :: [Int] -> Int -> Bool
hasSum ls sum = (length $ take 1 [(x,y) | x <- ls, y <- ls, y+x==sum]) > 0

findWeakness :: [Int] -> [(Int, Int)] -> Int
findWeakness ls [] = error "Weakness not found"
findWeakness ls ((i, x):xs) = if hasSum prevs x then findWeakness ls xs else x
    where prevs = prev25 ls i

invalidNum nums = findWeakness nums (drop 25 $ zip [0..] nums)

subList :: [a] -> Int -> Int -> [a]
subList ls index size = take size $ drop index ls

findNum :: [Int] -> Int -> Int -> [Int]
findNum ls num index = fn 1
    where fn size 
            | (sum slist > num)         = []      -- Not Found 
            | (index+size > length ls)  = []
            | (sum slist == num)        = slist   -- Found
            | otherwise                 = fn (size+1)
            where slist = subList ls index size 

findSeq :: [Int] -> Int -> [Int]
findSeq ls num = f 0
    where f index 
            | (index >= length ls)      = error "No seq found"
            | (length seq > 0)          = seq
            | otherwise                 = f (index+1)
            where seq = findNum ls num index


main = do
    input <- lines <$> readFile "input.txt"
    nums <- pure $ map read input :: IO [Int] 
    invNum <- pure $ invalidNum nums
    seq <- pure $ findSeq nums invNum
    (x,y) <- pure $ (minimum seq, maximum seq)
    return (x+y)
