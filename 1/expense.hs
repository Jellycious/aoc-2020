import System.IO
-- First Challenge
f2020 xs = [x * y | x <- xs, y <- xs, x + y == 2020] 
-- Second Challenge
g2020 xs = [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020] 

main = do
    content <- (fmap read <$> lines <$> readFile ("input.txt")) :: IO [Int]
    solution <- pure $ take 1 $ g2020 content
    sequence $ (putStrLn . show) <$> solution
