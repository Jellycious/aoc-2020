data Spot = Open | Tree
    deriving(Show, Eq)

toSpot c
    | c == '.'  = Open
    | c == '#'  = Tree

repeatingList ls = ls ++ repeatingList ls

parseInput :: [String] -> [[Spot]]
parseInput input = fmap (\r -> repeatingList (toSpot <$> r)) input

-- first-part Solution
myTraverse map = myTraverse' map 0
    where myTraverse' [] x = 0
          myTraverse' (l:ls) x
            | l!!x == Tree   = 1 + myTraverse' ls (x+3)
            | l!!x == Open   = myTraverse' ls (x+3)

main = do
    input <- lines <$> readFile ("input.txt") :: IO [String]
    result <- pure $ myTraverse $ parseInput input
    return result
