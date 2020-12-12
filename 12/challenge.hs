import Text.Parsec
import Data.Either

data Action = F Int | R Int| L Int | N Int | E Int | S Int | W Int
    deriving (Show, Eq)

data Facing = East | West | South | North
    deriving (Show)

-- Parsing
parseN = (const N) <$> char 'N'
parseS = (const S) <$> char 'S'
parseE = (const E) <$> char 'E'
parseW = (const W) <$> char 'W'
parseL = (const L) <$> char 'L'
parseR = (const R) <$> char 'R'
parseF = (const F) <$> char 'F'
parseAction = parseN <|> parseS <|> parseE <|> parseW <|> parseL <|> parseR <|> parseF
parseNum :: Parsec String () Int
parseNum = (read) <$> many1 digit
actionParser = parseAction <*> parseNum
-- End of Parsing

right East = South
right South = West
right West = North
right North = East

left East = North
left North = West
left West = South
left South = East

forward (East) n = (n, 0)
forward (South) n = (0, n)
forward (West) n = (-n, 0)
forward (North) n = (0, -n)
addPos (x, y) (x', y') = (x+x', y+y')

move (F x) (f, pos) = (f, addPos pos (forward f x))
move (N x) (f, pos) = (f, addPos pos (forward North x))
move (E x) (f, pos) = (f, addPos pos (forward East x))
move (S x) (f, pos) = (f, addPos pos (forward South x))
move (W x) (f, pos) = (f, addPos pos (forward West x))
move (R x) (f, pos) = (f' f, pos)
    where f' = foldr (.) id (replicate (x `div` 90) right)
move (L x) (f, pos) = (f' f, pos)
    where f' = foldr (.) id (replicate (x `div` 90) left)
   

main = do
    content <- lines <$> readFile "input.txt"
    parsed <- return $ map (fromRight (R (-1))) $ map (parse actionParser "") content
    (f, (x, y)) <- return $ foldl (flip move) (East, (0, 0)) parsed
    return (abs x + abs y)
