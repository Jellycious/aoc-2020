import Data.List
import Data.List.Split

composeGroups :: [String] -> [String]
composeGroups ls = composeGroups' ls []
    where   composeGroups' [] acc       = acc:[] 
            composeGroups' ("":xs) acc  = acc:(composeGroups' xs [])
            composeGroups' (x:xs) acc   = composeGroups' (xs) (acc++x)

removeDups ls = nub ls



main = do
    input <- (composeGroups . lines) <$> readFile "input.txt"
    cnt <- pure $ sum $ (length . removeDups) <$> input
    return cnt
