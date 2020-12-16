import Text.Parsec

numParser :: Parsec String () Int
numParser = (read) <$> many1 digit
numsParser = numParser `sepBy` char ','
rangeParser = (R) <$> numParser <*> (char '-' *> numParser)
rangesParser = rangeParser `sepBy` string " or "
ruleParser = ((many1 letter) `sepBy` space) *> (string ": " *> rangesParser)
rulesParser = ruleParser `sepEndBy` newline

yourTicketParser = string "your ticket:" *> newline *> numsParser <* newline
ticketsParser = string "nearby tickets:" *> newline *> (init <$> numsParser `sepEndBy` newline)

mainParser = (,,) <$> (rulesParser <* newline) <*> (yourTicketParser <* newline) <*> (ticketsParser)
-- END OF PARSING
type Rule =  [Range]
data Range = R Int Int
    deriving Show

checkRule [] x = False
checkRule (r:rs) x = if inRange r x then True else checkRule rs x 
inRange (R s e) x = x >= s && x <= e

validForAnyField :: [Rule] -> Int -> Bool
validForAnyField [] x = False 
validForAnyField (r:rs) x = if checkRule r x then True else validForAnyField rs x


main = do
    content <- readFile "testinput.txt"
    parsed <- return $ parse mainParser "" content
    (rules, yt, nts) <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    errors <- return $ filter (not . validForAnyField rules) (concat nts)
    return $ sum errors

