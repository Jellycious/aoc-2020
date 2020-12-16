import Text.Parsec
import Text.Regex.TDFA

import qualified Data.List as L
import qualified Data.Map as M

-- Far from pretty, but eh, it does the job.

data Rule =  F String [Range]
    deriving (Show)
data Range = R Int Int
    deriving (Show, Eq)

instance Eq Rule where
    (F k rs) == (F m ms)    = rs == ms

numParser :: Parsec String () Int
numParser = (read) <$> many1 digit
numsParser = numParser `sepBy` char ','
rangeParser = (R) <$> numParser <*> (char '-' *> numParser)
rangesParser = rangeParser `sepBy` string " or "
ruleParser = (F) <$> fieldNameParser <*> (string ": " *> rangesParser)
rulesParser = ruleParser `sepEndBy` newline
fieldNameParser = (many1 (letter <|> char ' '))

yourTicketParser = string "your ticket:" *> newline *> numsParser <* newline
ticketsParser = string "nearby tickets:" *> newline *> (init <$> numsParser `sepEndBy` newline)

mainParser = (,,) <$> (rulesParser <* newline) <*> (yourTicketParser <* newline) <*> (ticketsParser)
-- END OF PARSING

checkRanges :: [Range] -> Int -> Bool
checkRanges [] x = False
checkRanges (r:rs) x = if inRange r x then True else checkRanges rs x 
checkRule (F k rs) = checkRanges rs

inRange (R s e) x = x >= s && x <= e

validForAnyField :: [Rule] -> Int -> Bool
validForAnyField [] x = False 
validForAnyField ((F k r):rs) x = if checkRanges r x then True else validForAnyField rs x

validTicket rules [] = True 
validTicket rules (x:xs) = if validForAnyField rules x then validTicket rules xs else False

ruleAcceptsValues :: [Int] -> Rule -> Bool
ruleAcceptsValues vals rule = all (checkRule rule) vals

rulesAcceptingValues :: [Rule] -> [Int] -> [Rule]
rulesAcceptingValues rules vals = filter (ruleAcceptsValues vals) rules

solveRule rmap smap = if null rmap then smap else solveRule map' smap'
    where (i, r) = (\(i ,r) -> (i, head r)) $ head $ M.toList $ M.filter (\rs -> length rs == 1) rmap
          smap'  = M.insert i r smap
          map'   = M.map (L.delete r) (M.delete i rmap)

myTicketVal [] rmap = 1 
myTicketVal ((i, v):xs) rmap = if containsDepartures (k $ rmap M.! i) then v * myTicketVal xs rmap else 1 * myTicketVal xs rmap
    where k (F kk rs) = kk

containsDepartures :: String -> Bool
containsDepartures s = s =~ "departure" :: Bool

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse (mainParser) "" content
    (rules, yt, nts) <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    validnts <- return $ L.transpose $ filter (validTicket rules) nts
    rmap <- return $ foldl (\m (vs, i) -> M.insert i (rulesAcceptingValues rules vs) m) M.empty (zip validnts [0..])
    solvedRules <- return $ solveRule rmap M.empty  -- Solve the rules
    return $ myTicketVal (zip [0..] yt) solvedRules 

