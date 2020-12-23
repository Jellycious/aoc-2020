import qualified Data.Map as M

{-# LANGUAGE BangPatterns #-}

testinput = [3,8,9,1,2,5,4,6,7]
input = [4,6,9,2,1,7,5,3,8]

-- assumes input length of 9
instantiateMap input = foldl (\m v -> M.insert v (v+1) m) (inst input m') [10..999999]
    where m = M.singleton (1000000) (head input)
          m' = M.insert (last input) 10 m
          inst (a:[]) m = m
          inst (a:b:ls) m = inst (b:ls) (M.insert a b m)

ncup :: Int -> Int
ncup cup = ((cup-2) `mod` 1000000) + 1
ncup1 :: Int -> [Int] -> Int
ncup1 cup picked = head $ dropWhile ((flip elem) picked)  $ drop 1 $ (iterate ncup cup)

simulate :: Int -> M.Map Int Int -> (M.Map Int Int, Int)
simulate cur_cup m = (m', p4)
    where p1 = m M.! cur_cup
          p2 = m M.! p1
          p3 = m M.! p2
          p4 = m M.! p3
          n' = ncup1 cur_cup (p1:p2:p3:[])
          n'' = m M.! n'
          m' = M.insert p3 n'' $ M.insert n' p1 $ M.insert cur_cup p4 m

simulateN cur_cup m n = sim n (m,cur_cup)
    where sim 0 !(m, cur_cup) = (m,cur_cup)
          sim x !(m, cur_cup) = sim (x-1) (simulate cur_cup m)


main = do
    inp <- return $ input
    putStrLn $ show "Input: "++(show inp)
    m <- return $ instantiateMap inp
    (m', ncur) <- return $ simulateN (head inp) m 10000000
    n1 <- return $ m' M.! 1
    n2 <- return $ m' M.! n1
    putStrLn $ show (n1 * n2)
    return $ 0


