testinput = [3,8,9,1,2,5,4,6,7]
input = [4,6,9,2,1,7,5,3,8]

simulateN cups n = sim n (cups, cups!!0)
    where sim 0 (cups, cur_cup) = cups 
          sim x (cups, cur_cup) = sim (x-1) (simulate cups cur_cup)

simulate cups cur_cup = (ncups, new_cur_cup)
    where cs = take 9 $ dropWhile(/=cur_cup) (cycle cups)
          picked = take 3 $ drop 1 $ cs
          rem = (drop 4 $ cs) ++ [cur_cup]
          dest d = if d `elem` picked then dest (((d-2) `mod` 9) + 1) else d 
          destination = dest (((cur_cup-2) `mod` 9) + 1)
          (f,b) = span (/=destination) rem
          ncups = f ++ [head b] ++ picked ++ tail b
          new_cur_cup = head $ drop 1 $ dropWhile (/=cur_cup) (cycle ncups)

main = do
    inp <- return $ input
    scups <- return $ inp
    res <- return $ simulateN scups 100
    putStrLn $ "INPUT: "++(show inp)
    return $ (take 8 $ drop 1 $ (dropWhile (/=1) (cycle res)))


