import Text.Parsec
import Debug.Trace
import qualified Data.HashSet as H

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

fst' (a, _, _) = a
snd' (_, a, _) = a
thrd' (_, _, a) = a

data Winner = P1 | P2
    deriving (Eq, Show)

recursiveCombat p1@(a:as) p2@(b:bs) prevs
    | infinite                                  = (p1, P1, prevs) -- infinite loop prevention
    | (a <= (length as) && b <= (length bs))    = let (cs, win, prevsp) = subGame as bs a b in if win == P1 then trace ("\nround: "++(show ((as++(a:b:[])), bs))) recursiveCombat (as++(a:b:[])) bs prevsp  else trace ("\nround: "++(show (as, (bs++(b:a:[]))))) recursiveCombat as (bs++(b:a:[])) prevsp
    | (as == [])                                = (bs++(b:a:[]), P2, ((p1,p2):prevs))
    | (bs == [])                                = (as++(a:b:[]), P1, ((p1,p2):prevs))
    | (a > b)                                   = recursiveCombat (as++(a:b:[])) bs ((p1,p2):prevs)
    | otherwise                                 = recursiveCombat as (bs++(b:a:[])) ((p1,p2):prevs)
        where subGame p1 p2 c1 c2 = let p1' = take c1 p1
                                        p2' = take c2 p2
                                    in trace ("subgame: "++(show (p1',p2'))++(show (prevs))) $ recursiveCombat p1' p2' prevs
              infinite = (p1,p2) `elem` prevs

recCombat2 p1 p2 prevs



main = do
    content <- readFile "testinput.txt"
    parsed <- return $ parse parser "" content
    putStrLn $ show parsed
    p <- case parsed of
        (Right x) -> return x
        (Left x) -> error "Parse Error"
    game <- return $ recursiveCombat (fst p) (snd p) []
    putStrLn $ show $ (fst' game)
    putStrLn $ show $ length (fst' game)
    putStrLn $ show $ length (fst p)
    putStrLn $ show $ length (snd p)
    putStrLn $ show $ getScore (fst' game)
    return (0)
