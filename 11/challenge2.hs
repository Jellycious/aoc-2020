import Text.Parsec as P
import Control.Applicative
import Control.Exception
import Data.Either
import Data.List

-- Parsing
type Floor = [Row]
type Row = [Seat]

data Seat = F | O | L
    deriving(Show, Eq)

parseFloor = (const F) <$> char '.'
parseOccup = (const O) <$> char '#'
parseAvailable = (const L) <$> char 'L'
parseSeat = parseFloor P.<|> parseAvailable P.<|> parseOccup
parseRow :: Parsec String () [Seat]
parseRow = P.many parseSeat

-- Solving
getPos :: Floor -> Int -> Int -> [Seat]
getPos floor (-1) c = [] 
getPos floor r (-1) = []
getPos floor r c 
    | (r >= length floor)               = []
    | (c >= length (floor!!0))   = []
    | otherwise                         = [floor!!r!!c]


firstVisible floor r c d
    | (n == [])         = F
    | (head n == F)     = firstVisible floor r' c' d  
    | (head n == O)     = O
    | (head n == L)     = L
    where (r', c')  = (r + fst d, c + snd d)
          n         = getPos floor r' c'

-- This is changed to the first seats that are visible
getSurrounding floor r c = map (firstVisible floor r c) [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]


changeNode :: Floor -> (Int, Int) -> (Seat, Bool)
changeNode floor (r, c)
    | (s == F)      = (F, False)
    | (s == L)      = if c1 then (O, True) else (L, False) 
    | (s == O)      = if c2 then (L, True) else (O, False) 
    where   s   = head $ getPos floor r c
            n   = getSurrounding floor r c
            c1  = (length . filter (==O)) n == 0 
            c2  = (length . filter (==O)) n >= 5

-- Simulate one round and keep track of whether there is a change
simulate :: Floor -> (Floor, Bool)
simulate floor = (floor', c')
    where   rsize       = (length floor) - 1
            csize       = (length (floor!!0)) - 1
            enumer      = map (\r -> zip (repeat r) [0..csize]) [0..rsize]
            changed     = map (map (changeNode floor)) enumer
            changed'    = map (unzip) changed
            floor'      = map (fst) changed'
            c           = map (snd) changed'
            c'          = foldl (||) False $ map (foldl (||) False) c

simulation :: (Floor, Bool) -> Floor
simulation (f, False) = f
simulation (f, True) = simulation $ simulate f


main = do
    parsed <- (map (parse parseRow "")) <$> lines <$> readFile "input.txt"
    floor <- (return $ map (fromRight []) parsed) :: IO Floor
    c <- return $ simulation (floor, True)
    putStrLn $ show . sum $ (length . filter (==O)) <$> c

