import Text.Parsec
import Debug.Trace
import qualified Data.HashSet as H

-- PARSING
numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

numsParser = numParser `endBy` newline

playerParser :: Parsec String () [Int]
playerParser = ((many1 letter) *> char ' ' *> digit *> char ':' *> newline) *> numsParser

parser = (,) <$> playerParser <*> (newline *> playerParser)
-- END OF PARSING

getScore :: [Int] -> Int
getScore ls = sum $ zipWith (*) (reverse ls) [1..] 

data Winner = P1 | P2
    deriving (Eq, Show)

-- Nested if expression HORROR
recCombat :: [Int] -> [Int] -> H.HashSet ([Int],[Int]) -> ([Int],Winner,H.HashSet ([Int],[Int]))
recCombat p1 p2 prevs = if H.member (p1,p2) prevs then (p1,P1,prevs) 
                        else if length p1 == 0 then (p2,P2,prevs)
                        else if length p2 == 0 then (p1,P1,prevs)
                        else
                            (if (length d1' >= c1 && length d2' >= c2) then 
                                let (wd,win,prevsg) = recCombat (take c1 d1') (take c2 d2') prevs' in
                                    if win == P1 then recCombat (d1'++[c1,c2]) d2' prevs' else recCombat d1' (d2'++[c2,c1]) prevs'
                            else 
                                if c1 > c2 then recCombat (d1'++[c1,c2]) (d2') (prevs') else recCombat (d1') (d2'++[c2,c1]) prevs'
                            )
    where prevs' = H.insert (p1,p2) prevs
          c1 = head p1
          d1' = tail p1
          c2 = head p2
          d2' = tail p2

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse parser "" content
    putStrLn $ show parsed
    p <- case parsed of
        (Right x) -> return x
        (Left x) -> error "Parse Error"
    (wd, winner, hmap) <- return $ recCombat (fst p) (snd p) H.empty
    return $ getScore wd
