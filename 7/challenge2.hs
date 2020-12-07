import Text.Regex.TDFA
import qualified Data.Map as M

data BagRule =  Empty String | Rule String Rule  
    deriving (Show)

type Rule = [(Int, String)]

testRules = ["light red bags contain 2 muted yellow bags.","muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.","faded blue bags contain no other bags."]

parseBagRule :: String -> BagRule
parseBagRule l = if empty then Empty col else Rule col rules 
    where   col = take ((length reg) - 13) reg 
            rule = drop (1 + length reg) l 
            empty = (rule =~ "no other bags") :: Bool
            reg = (l =~ "^[a-zA-Z ]+ contain") :: String
            rules = parseRule rule 

parseRule :: String -> [(Int, String)]
parseRule l = map (splt . clnp) rs
    where   rs      = getAllTextMatches (l =~ "([0-9]+){1} [a-zA-Z ]+") :: [String]
            clnp :: String -> String
            clnp r  = getBefore $ ((r =~ " bag(s?)") :: (String, String, String))
            splt :: String -> (Int, String)
            splt r  = (cnt r, col r) 
            cnt :: String -> Int
            cnt r   = read ((r =~ "[0-9]+") :: String) :: Int
            col :: String -> String
            col r   = drop 1 $ (r =~ " [a-zA-Z ]+") :: String

getBefore (b, _, _) = b

convMap :: BagRule -> M.Map String (Maybe Rule)
convMap br = case br of 
                (Empty col) -> M.singleton col Nothing
                (Rule col r) -> M.singleton col (Just r)

createRuleMap :: [BagRule] -> M.Map String (Maybe Rule)
createRuleMap rules = M.unions $ map convMap rules 

shinyBagCount :: M.Map String (Maybe Rule) -> String -> Int
shinyBagCount rmap key = case (rmap M.! key) of
                            Nothing -> 1 
                            Just r  -> 1 + (sum $ (map f r))
                        where f s = (fst s) * shinyBagCount rmap (snd s) 

main = do
    input <- lines <$> readFile "input.txt"
    rulemap <- return $ createRuleMap $ parseBagRule <$> input
    answer <- return $ shinyBagCount rulemap "shiny gold"
    return (answer - 1)
