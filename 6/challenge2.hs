import Data.List
import Data.List.Split

composeGroups :: [String] -> [String]
composeGroups (x:xs) = composeGroups' xs x
    where   composeGroups' [] acc       = acc:[] 
            composeGroups' ("":x:xs) acc  = acc:(composeGroups' xs x)
            composeGroups' (x:xs) acc   = composeGroups' (xs) (acc++"\n"++x)

removeDups ls = nub ls

chars = ['a'..'z']
testGroup = ["xav","uavx","xavsi","yavx"]

intersector group = length $ intersector' group chars
    where   intersector' [] acc = acc 
            intersector' (x:xs) acc = intersector' xs (intersect acc x)



main = do
    input <- (composeGroups . lines) <$> readFile "input.txt"
    groups <- pure $ lines <$> input
    cnt <- pure $ sum $ intersector <$> groups
    return cnt
