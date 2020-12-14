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

data F = F Int Int
    deriving (Show)

-- Works because id1 and id2 are coprime
mylcm id1 id2 = id1 * id2

sequencer (F x y) = [x * i + y | i <- [0..]]

addReq (F x y) (u, t) = F (mylcm x u) (firstIntersect (F x y) (u,t))

firstIntersect f (u, t) = head $ filter req (sequencer f) 
    where req e = (e+t) `mod` u == 0

main = do
    content <- lines <$> readFile "input.txt"
    solve content

solve content = do 
    timestamp <- return $ read (content!!0) :: IO Int
    requirements <- return $ getRequirements $ parseSchedule (content!!1)
    (uid, ttw) <- return $ head requirements
    initReq <- return $ F uid ttw   -- This only works because the first ttw is 0
    seq <- return $ foldl addReq initReq (tail requirements)
    return $ head $ sequencer seq 
    
