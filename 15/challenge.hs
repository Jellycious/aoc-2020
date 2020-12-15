import Text.Parsec
import Debug.Trace

numParser :: Parsec String () Int
numParser = (read) <$> many1 digit
numsParser = numParser `sepBy` char ','
-- END OF PARSING

playGame 2020 ls = nextNum 2020 ls
playGame i ls = (playGame (i+1) (n:ls))
    where n = nextNum i ls

nextNum i (n:ns)
    | ((fst n) `elem` ns1)  = (snd n - pi, i) 
    | otherwise             = (0, i)
        where ns'       = unzip ns
              ns1       = fst ns'
              ns2       = snd ns'  
              (pn, pi)  = head $ filter ((==(fst n)) . fst) ns 


main = do
    content <- readFile "input.txt"
    parsed <- return $ parse numsParser "" content
    nums <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    putStrLn $ show $ fst $ playGame ((length nums) + 1) (reverse $ zip nums [1..])

