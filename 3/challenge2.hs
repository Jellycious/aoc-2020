-- Second-Part Solution
myTraverse :: Int -> Int -> [String] -> Int
myTraverse xoff yoff map = myTraverse' (cycle <$> map) 0
    where myTraverse' [] x = 0
          myTraverse' (l:ls) x
            | l!!x == '#'   = 1 + myTraverse' (drop yoff (l:ls)) (x+xoff)
            | l!!x == '.'   = myTraverse' (drop yoff (l:ls)) (x+xoff)

main = do
    input <- lines <$> readFile ("input.txt") :: IO [String]
    results <- pure $ map (\(x, y) -> myTraverse x y input) [(1,1),(3,1),(5,1),(7,1),(1,2)]
    return $ product results

