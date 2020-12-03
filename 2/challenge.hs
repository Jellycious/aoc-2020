import System.IO
-- Let's use REGEX for the next challenge, this is ugly.

count c ([]) = 0 
count c (x:xs) = if x==c then 1 + count c xs else count c xs 

splitBy :: Char -> String -> (String, String)
splitBy c str = splitBy' c str []
    where splitBy' c [] ys      = (ys, []) 
          splitBy' c (x:xs) ys  = if x == c then (ys, xs) else splitBy' c xs (ys++[x])

getL (l,_,_,_) = l
getH (_,h,_,_) = h
getC (_,_,c,_) = c
getW (_,_,_,w) = w


parse s = (low, high, character!!0, tail word)
    where (range, rest) = splitBy ' ' s
          (lows, highs) = splitBy '-' range
          (low, high) = (read lows :: Int, read highs :: Int)
          (character, word) = splitBy ':' rest

validPass :: String -> Bool
validPass s = cnt `elem` [low..high]
    where (low, high, c, w) = parse s
          cnt = count c w

main = do
    content <- (lines <$> readFile ("input.txt")) :: IO [String]
    correct <- pure $ count True (validPass <$> content)
    return correct
