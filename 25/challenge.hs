import Text.Parsec
import Data.Maybe
import Data.List

{-# LANGUAGE BangPatterns #-}

numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

parser = (,) <$> numParser <*> (newline *> numParser)

f y x = (x * y) `mod` 20201227

findLoopSize x = (+2) <$> fromJust $ findIndex (==x) res
    where res = tail $ iterate (f 7) 7

trans :: (Int -> Int) -> Int -> Int -> Int
trans f !x 1 = x  
trans f !x loopsize = trans f (f x) (loopsize-1)

main = do
    input <- readFile "input.txt"
    parsed <- return $ parse parser "" input
    (cpk, dpk) <- case parsed of
        (Right x) -> return x
        (Left x) -> error "Parser Error"
    (clz, dlz) <- return $ (,) (findLoopSize cpk) (findLoopSize dpk)
    putStrLn $ show $ (clz, dlz)
    enc_key <- return $ trans (f cpk) cpk (dlz)
    putStrLn $ show $ enc_key
    return enc_key
    
