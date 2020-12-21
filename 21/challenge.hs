import Text.Parsec
import qualified Data.Map as M
import qualified Data.HashSet as H
import Data.List
testIn = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
-- PARSING
ingredient :: Parsec String () String
ingredient = many1 letter
ingredients = ingredient `endBy` char ' '
ingredients :: Parsec String () [String]
allergens = (many1 letter) `sepEndBy` string ", "
allergensList = string "(contains " *> (allergens <* char ')')
item = (,) <$> ingredients <*> allergensList
parser = item `sepEndBy` char '\n'



testParser = parse parser "" testIn
-- END OF PARSING

allergenMap :: [([String],[String])] -> M.Map String (H.HashSet String) -> M.Map String (H.HashSet String)
allergenMap [] m = m
allergenMap (p:ps) m = allergenMap ps m''
    where (ingredients, allergens) = p
          m' = foldl (\m i -> if M.notMember i m then M.insert i H.empty m else m) m ingredients
          m'' = foldl (\m i -> M.adjust (\h -> foldl (\h a -> H.insert a h) (h) allergens) i m) m' ingredients

allergenMap2 [] m = m
allergenMap2 (p:ps) m = allergenMap2 ps m''
    where (ingredients, allergens) = p
          m' = foldl (\m i -> if M.notMember i m then M.insert i [] m else m) m allergens
          m'' = foldl (\m i -> M.adjust (++[ingredients]) i m) m' allergens

eliminateIngredients amap = M.map (\a -> if length a > 1 then foldl (intersect) (head a) (tail a) else head a) amap

solve amap
    | (M.size amap > 0)     = (aller, i):(solve m')
    | otherwise = []
        where (aller, i) = head $ map (\(aller, li) -> (aller, head li)) $ M.toList $ M.filter (\a -> (length a <= 1)) amap
              m' = M.map (\a -> delete i a) (M.delete aller amap)

countOccur _ [] = 0
countOccur x list = sum $ map (\a -> 1) $ filter (==x) list


main = do
    content <- parse parser "" <$> readFile "input.txt"
    p <- case content of 
                (Right x) -> return $ x
                (Left x) -> putStrLn (show x) >> error "Parse Error"
    ingredients <- return $ M.keys $ allergenMap p M.empty
    allergMap <- return $ allergenMap2 p M.empty
    m <- return $ eliminateIngredients allergMap
    sol <- return $ solve m
    ingredientsListing <- return $ foldl (++) [] (map fst p)
    ingredientsNoAllergen <- return $ foldl (\i e -> delete e i) (ingredients) (map snd sol)
    putStrLn $ show $ sortBy (\a1 a2 -> compare (fst a1) (fst a2)) sol
    return $ sum $ map ((flip countOccur) ingredientsListing) ingredientsNoAllergen
    
