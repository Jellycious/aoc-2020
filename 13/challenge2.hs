import Text.Regex.TDFA
import Data.Bool
import Data.List

parseSchedule :: String -> [String]
parseSchedule s = getAllTextMatches $ s =~ "[0-9x]+" :: [String]

getRequirements s = getRequirements' s [] 0
getRequirements' [] rs a = rs
getRequirements' (x:xs) rs a
    | (x == "x")    = getRequirements' xs rs (a+1)      
    | otherwise     = getRequirements' xs (rs++[(uid, a)]) (a+1) 
        where uid = read x :: Int

-- END OF PARSING

-- Observations: 
--      The bus id's are all coprime.
--      The first equation can be written as uid*i + 0 (forall i >= 0) 

sequencer (x,y) = [x*i+y | i <- [0..]]

addReq (x,y) (u,t) = (x*u, newOffset (x,y) (u,t))

newOffset f (u,t) = head $ filter req (sequencer f) 
    where req e = (e+t) `mod` u == 0

main = do
    content <- lines <$> readFile "input.txt"
    solve content

solve content = do 
    timestamp <- return $ read (content!!0) :: IO Int
    requirements <- return $ getRequirements $ parseSchedule (content!!1)
    seq <- return $ foldl addReq (head requirements) (tail requirements)
    return $ head $ sequencer seq 
