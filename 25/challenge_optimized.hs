import Text.Parsec
import Data.Maybe
import Data.List

numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

parser = (,) <$> numParser <*> (newline *> numParser)

f y x = (x * y) `mod` 20201227

findLoopSize x = find 1 0
    where g     = f 7 7
          find 1 prv = if g == x then 2 else find 2 g
          find l prv = let v = (f 7) prv in if v == x then l+1 else find (l+1) v 

trans x loopsize = powm x loopsize 20201227 1

-- Modular Exponentation taken from: https://rosettacode.org/wiki/Modular_exponentiation
powm :: Int -> Int -> Int -> Int -> Int
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

main = do
    input <- readFile "input.txt"
    parsed <- return $ parse parser "" input
    (cpk, dpk) <- case parsed of
        (Right x) -> return x
        (Left x) -> error "Parser Error"
    clz <- return $ findLoopSize cpk
    putStrLn $ show $ clz
    enc_key <- return $ trans dpk clz
    putStrLn $ show $ enc_key
    return enc_key
