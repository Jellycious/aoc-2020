import Data.List.Split
import Text.Regex.TDFA
--First-part Solution
splitPassports :: [String] -> [String]
splitPassports input = splitPassports' [] input
    where splitPassports' acc [] = [acc] 
          splitPassports' acc (x:xs) 
                |   x == ""   = acc:(splitPassports' [] xs)
                |   otherwise = splitPassports' (acc++" "++x) xs 

testInput = ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd","byr:1937 iyr:2017 cid:147 hgt:183cm","","iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884","hcl:#cfa07d byr:1929"]

test = splitPassports $ testInput

keysValues :: String -> [String]
keysValues p = tail $ splitOn " " p


getKeys :: [String] -> [String]
getKeys kv = map (\a -> init (a =~ "[a-zA-Z]+:" :: String)) kv

getValue = 

requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

validKeys :: String -> Bool
validKeys passport = foldl (&&) True $ map (flip elem keys)  requiredKeys
    where keys = (getKeys . keysValues) passport

countValidPP passports = length $ filter (==True) $ map validKeys passports

checkByr val = 
    where num = val :: Int



main = do
    passports <- splitPassports <$> lines <$> readFile "input.txt"
    count <- pure $ countValidPP passports
    return count
