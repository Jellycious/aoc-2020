import Text.Parsec
import Data.Map
import qualified Data.List as L

numParser :: Parsec String () Int
numParser = (read) <$> many1 digit
numsParser = numParser `sepBy` char ','
-- END OF PARSING

-- Uses a map as new solution.
-- Requires XBangPatterns flag set to prevent stack overflow.
playGame 30000000 ln m = nextNum ln m
playGame !i !ln !m = playGame (i+1) n m'
    where n     = nextNum ln m
          m'    = updateMap i n m 

updateMap i n m 
    | (member n m)  = adjust (updateA i) n m 
    | otherwise     = insert n [i] m

updateA i [] = [i] 
updateA i is = L.take 2 $ i:is

nextNum ln m 
    | (length a == 2)   = ((a L.!! 0) - (a L.!! 1))
    | otherwise         = 0
    where a = (m ! ln)



main = do
    content <- readFile "input.txt"
    parsed <- return $ parse numsParser "" content
    nums <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    mmap <- return $ Prelude.foldl (\m (n, i) -> insert n [i] m) empty (zip nums [1..])
    putStrLn $ show $ playGame ((length nums) + 1) (last nums) mmap

