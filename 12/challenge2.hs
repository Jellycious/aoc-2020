import Text.Parsec
import Data.Either

data Action = F Int | R Int| L Int | N Int | E Int | S Int | W Int
    deriving (Show, Eq)

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

right (x, y) = (-y, x)
left (x, y) = (y, -x)

forward (wx, wy) (sx, sy) n = (sx+(n*wx), sy+(n*wy))
addPos (x, y) (x', y') = (x+x', y+y')

move (F x) (wpos, spos) = (wpos, forward wpos spos x)
move (N x) (wpos, spos) = (addPos wpos (0, -x), spos)
move (E x) (wpos, spos) = (addPos wpos (x, 0), spos)
move (S x) (wpos, spos) = (addPos wpos (0, x), spos)
move (W x) (wpos, spos) = (addPos wpos (-x, 0), spos)
move (R x) (wpos, spos) = ((foldr (.) id (replicate (x `div` 90) right)) wpos, spos)
move (L x) (wpos, spos) = ((foldr (.) id (replicate (x `div` 90) left)) wpos, spos)
   

main = do
    content <- lines <$> readFile "input.txt"
    parsed <- return $ map (fromRight (R (-1))) $ map (parse actionParser "") content
    (w, (x, y)) <- return $ foldl (flip move) ((10, -1), (0, 0)) parsed
    return (abs x + abs y)
