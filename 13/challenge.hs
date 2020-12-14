import Text.Regex.TDFA

parseSchedule :: String -> [Int]
parseSchedule s = read <$> m :: [Int] 
    where m = getAllTextMatches $ s =~ "[0-9]+" :: [String]

timeToWait timestamp scheduletime
    | (timestamp `mod` scheduletime == 0)   = (0, scheduletime)
    | otherwise                             = ((scheduletime * k) - timestamp, scheduletime)
        where k = ((timestamp `div` scheduletime)+1)

main = do 
    content <- lines <$> readFile "input.txt"
    timestamp <- return $ read (content!!0) :: IO Int
    (s:ss) <- return $ parseSchedule (content!!1)
    (t, u) <- return $ foldl (\(t, u) st -> let (ttw, uid) = timeToWait timestamp st in if ttw < t then (ttw, uid) else (t, u)) (timeToWait timestamp s) ss
    return (t*u) 
