import Text.Parsec

numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

numsParser = numParser `endBy` newline

playerParser :: Parsec String () [Int]
playerParser = ((many1 letter) *> char ' ' *> digit *> char ':' *> newline) *> numsParser

parser = (,) <$> playerParser <*> (newline *> playerParser)
-- END OF PARSING

playGame [] p2 = p2
playGame p1 [] = p1
playGame (a:as) (b:bs)
    | (a > b)   = playGame (as++(a:b:[])) bs
    | otherwise = playGame as (bs++(b:a:[]))

getScore :: [Int] -> Int
getScore ls = sum $ zipWith (*) (reverse ls) [1..] 

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse parser "" content
    p <- case parsed of
        (Right x) -> return x
        (Left x) -> error "Parse Error"
    game <- return $ playGame (fst p) (snd p)
    return $ getScore game
