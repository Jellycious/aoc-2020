import Text.Parsec
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Either as E

-- PARSING
sp :: Parsec String () Char
sp = char ' '
numParser :: Parsec String () Int
numParser = read <$> (many1 digit)
ruleNum = numParser <* string ": "

conParser = C <$> (numParser `sepEndBy1` sp)
orParser = O <$> (conParser `sepBy1` string "| ")
tParser = (between (char '"') (char '"') (letter))
ruleParser = try (T <$> ruleNum <*> tParser) <|> (R <$> ruleNum <*> orParser) 
rulesParser = ruleParser `endBy` newline

candParser :: Parsec String () [String]
candParser = (many1 letter) `endBy` newline

parser = ((,) <$> rulesParser) <*> (newline *> candParser)
-- END OF PARSING

data Rule = R Int Or | T Int Char
    deriving Show
data Or = O [Con]
    deriving Show
data Con = C [Int]
    deriving Show

getRuleKey (R x o) = x
getRuleKey (T x o) = x

checkRule' :: M.Map Int Rule -> Rule -> String -> [String]
checkRule' m (T k c) s
    | (length s > 0)    = if head s == c then [tail s] else []
    | otherwise = [] 

checkRule' m (R k o) s = checkOr m o s

checkCon :: M.Map Int Rule -> Con -> String -> [String]
checkCon m (C []) s = return s
checkCon m (C (r:rs)) s = do
    s' <- checkRule' m (m M.! r) s
    checkCon m (C rs) s'

checkOr :: M.Map Int Rule -> Or -> String -> [String]
checkOr m (O []) s = []
checkOr m (O cs) s = concat $ map (\c -> checkCon m c s) cs

createRuleMap :: [Rule] -> M.Map Int Rule
createRuleMap rules = foldl (\m r -> M.insert (getRuleKey r) r m) M.empty rules

main = do 
    content <- readFile "input2.txt"
    parsed <- return $ parse parser "" content
    (rules, cands) <- case parsed of 
      (Right x) -> return $ x
      (Left x) -> putStrLn (show x) >> error "Parse Error"
    rmap <- return $ createRuleMap rules
    legit <- return $ map (checkRule' rmap (rmap M.! 0)) cands
    return $ length $ filter (\ls -> "" `elem` ls) legit


