import Text.Regex.TDFA
import Data.Bool

parseSchedule :: String -> [String]
parseSchedule s = getAllTextMatches $ s =~ "[0-9x]+" :: [String]

getRequirements s = getRequirements' s [] 0
getRequirements' [] rs a = rs
getRequirements' (x:xs) rs a
    | (x == "x")    = getRequirements' xs rs (a+1)      
    | otherwise     = getRequirements' xs (rs++[(uid, a)]) (a+1) 
        where uid = read x :: Int


timeToWait timestamp scheduletime
    | (timestamp `mod` scheduletime == 0)   = (0, scheduletime)
    | otherwise                             = ((scheduletime * k) - timestamp, scheduletime)
        where k = ((timestamp `div` scheduletime)+1)

assertReq :: (Int, Int) -> Int -> Bool
assertReq (uid, ttw) t = ((t+ttw) `mod` uid) == 0

combineReqs :: [Int -> Bool] -> (Int -> Bool)
combineReqs (f:[]) = f
combineReqs (f:fs) = \x -> f x && (combineReqs fs x)

-- NAIVE SOLUTION TOO SLOW
main = do 
    content <- lines <$> readFile "input.txt"
    timestamp <- return $ read (content!!0) :: IO Int
    requirements <- return $ getRequirements $ parseSchedule (content!!1)
    reqs <- return $ combineReqs $ assertReq <$> requirements
    sol <- return $ take 1 $ filter (reqs) [1..]
    return sol
