import Text.Parsec
import qualified Data.Map as M
import Data.Maybe

data Rule = R Int Or | T Int Char
    deriving Show
data Or = O [Con]
    deriving Show
data Con = C [Int]
    deriving Show

getRuleKey (R x o) = x
getRuleKey (T x o) = x

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

createRuleMap :: [Rule] -> M.Map Int Rule
createRuleMap rules = foldl (\m r -> M.insert (getRuleKey r) r m) M.empty rules

-- Returns the left-over string when successfull
-- If it returns Nothing the check was unsuccessfull
checkRule m r s = if checkRule' m r s == Just "" then True else False
checkRule' :: M.Map Int Rule -> Rule -> String -> Maybe String
checkRule' m (T k c) (x:xs) = if x == c then Just xs else Nothing
checkRule' m (R k o) s = checkOr m s o

checkCon m s (C []) = Just s
checkCon m s (C (k:ks)) = do
    s' <- checkRule' m (m M.! k) s
    checkCon m s' (C ks)

checkOr m s (O []) = Nothing
checkOr m s (O (c:cs)) = if isNothing t then checkOr m s (O cs) else t
    where t = checkCon m s c



main = do 
    content <- readFile "input.txt"
    parsed <- return $ parse parser "" content
    (rules, cands) <- case parsed of 
      (Right x) -> return $ x
      (Left x) -> putStrLn (show x) >> error "Parse Error"
    rmap <- return $ createRuleMap rules
    legit <- return $ map (checkRule rmap (rmap M.! 0)) cands
    return (length $ filter (==True) legit)


