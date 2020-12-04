import Data.List.Split
import Text.Regex.TDFA
--second-part Solution
splitPassports :: [String] -> [String]
splitPassports input = splitPassports' [] input
    where splitPassports' acc [] = [acc] 
          splitPassports' acc (x:xs) 
                |   x == ""   = acc:(splitPassports' [] xs)
                |   otherwise = splitPassports' (acc++" "++x) xs 

testInput = ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd","byr:1937 iyr:2017 cid:147 hgt:183cm","","iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884","hcl:#cfa07d byr:1929"]

testPassports = splitPassports $ testInput

keysValues :: String -> [String]
keysValues p = tail $ splitOn " " p


getKeys :: [String] -> [String]
getKeys kvs = map getKey kvs

getKey :: String -> String
getKey kv = init $ kv =~ "[a-zA-Z]+:" :: String
getValue :: String -> String
getValue kv = tail $ kv =~ ":[a-zA-Z0-9#]+" :: String

requiredKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

validKeys :: String -> Bool
validKeys passport = foldl (&&) True $ map (flip elem keys)  requiredKeys
    where keys = (getKeys . keysValues) passport

checkKey :: String -> String -> Bool
checkKey "byr" val = if fourdigits val then x >= 1920 && x <= 2002  else False
    where x = read val :: Int
checkKey "iyr" val = if fourdigits val then let x = read val :: Int in x >= 2010 && x <= 2020 else False
checkKey "eyr" val = if fourdigits val then let x = read val :: Int in x >= 2020 && x <= 2030 else False
checkKey "hgt" val
    |   (val =~ "^[0-9]+cm$" :: Bool)     = x >= 150 && x <= 193
    |   (val =~ "^[0-9]+in$" :: Bool)     = x >= 59 && x <= 76
    |   otherwise                       = False 
        where x = read (val =~ "[0-9]+" :: String) :: Int
checkKey "hcl" val = (val =~ "^#[0-9a-f]{6}$" :: Bool)
checkKey "ecl" val = val `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
checkKey "pid" val = (val =~ "^[0-9]{9}$") :: Bool
checkKey "cid" val = True

fourdigits :: String -> Bool
fourdigits v = (v =~ "^[0-9]{4}$" :: Bool)

checkPassport passport 
    |   validKeys passport      = foldl (&&) True cs
    |   otherwise               = False
        where   kvs = keysValues passport 
                cs = map (\p -> checkKey (getKey p) (getValue p)) kvs

test1 = checkPassport (testPassports!!0)
test2 = checkPassport (testPassports!!1)
                



main = do
    passports <- splitPassports <$> lines <$> readFile "input.txt"
    correct <- length $ filter (==True) (checkPassport <$> passport)
    return kvs
