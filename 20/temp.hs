import Text.Parsec
import Data.List
-- PARSING
numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

idParser = string "Tile " *> (numParser <* string ":\n")
tileParser = (const X) <$> (char '#') <|> (const O) <$> (char '.')
rowParser = many1 tileParser
rowsParser = rowParser `endBy1` newline
imgParser = Img <$> idParser <*> rowsParser
imgsParser = imgParser `sepBy1` newline

testInput = "Tile 1217:\n.#....#..#\n#.#....###\n##.#....#.\n##...###..\n###..#..#.\n.#...##...\n#.#..##...\n#...#.##..\n#..##....#\n.##.#..#.."
test = parse imgParser "" testInput

-- END OF PARSING
data Tile = X | O
    deriving (Show, Eq)

data Img = Img Int [[Tile]]

instance Show Img where
    show (Img x g) = "Tile: "++(show x)++"\n"++(printRows g)
        where printRows rs = concatMap (\r -> (concatMap (show) r)++"\n") rs

-- Used for storing borders of an image.
-- In the order of top, right, bottom, left
-- The horizontal borders are stored left to right.
-- The vertical borders top to bottom.
--
data Borders = B Int [Tile] [Tile] [Tile] [Tile]
    deriving Eq

instance Show Borders where
    show (B k t r b l) = "Borders Tile: "++(show k)++"\n"++(printBorders)
        where printBorders = ts ++ concatMap sm mid ++ bs
              ts = (concatMap show t)++"\n"
              bs = (concatMap show b)++"\n"
              sm (le, re) = (show le) ++ replicate ((length t) - 2) '#' ++ (show re) ++ "\n"
              mid = zip ((init . tail) l) ((init . tail) r)

data Sym = Id | R | R2 | R3 | TX | TY | TAC | TBD
    deriving (Show, Eq)

getGrid (Img k g) = g

getTop (B k t r b l) = t
getRight (B k t r b l) = r
getBottom (B k t r b l) = b
getLeft (B k t r b l) = l
getId (B k t r b l) = k

extractBorders (Img k g) = B k t r b l
    where g' = transpose g 
          t = head g
          b = last g
          l = head g' 
          r = last g'

sameBorder [] [] = True
sameBorder (x:xs) (y:ys)
    | (x==y)    = sameBorder xs ys
    | otherwise = False

-- List of symmetries excluding identity
tX (B k t r b l) = B k b (reverse r) t (reverse l)
tY (B k t r b l) = B k (reverse t) l (reverse b) r
r1 (B k t r b l) = B k r (reverse b) l (reverse t) 
r2 (B k t r b l) = B k (reverse b) (reverse l) (reverse t) (reverse r)
r3 (B k t r b l) = B k (reverse l) t (reverse r) b
t13 (B k t r b l) = B k l b r t
t24 (B k t r b l) = B k (reverse r) (reverse t) (reverse l) (reverse b)

symmetries b = (b:(tX b):(tY b):(r1 b):(r2 b):(r3 b):(t13 b):(t24 b):[])


-- Returns possible orientations of b which aligns with fixed top image bt
alignsT bt b = filter pred (symmetries b)
    where pred b = sameBorder (getBottom bt) (getTop b)
alignsB bb b = filter pred (symmetries b)
    where pred b = sameBorder (getTop bb) (getBottom b)
alignsR br b = filter pred (symmetries b)
    where pred b = sameBorder (getLeft br) (getRight b)
alignsL bl b = filter pred (symmetries b)
    where pred b = sameBorder (getRight bl) (getLeft b)

-- Finds the elements that could align with b, where b is fixed.
-- So for alignFunc == alignsT: it would find the elments which could have b as top element.
-- Requires b not in borders
findAligns alignFunc b borders = concatMap (alignFunc b) bs'
    where bs'   = deleteBy (\b b' -> (getId b) == (getId b')) b borders

neighbourCands b borders = (tns, rns, bns, lns)
    where tns = findAligns alignsB b borders -- Possible top neighbours
          rns = findAligns alignsL b borders
          bns = findAligns alignsT b borders
          lns = findAligns alignsR b borders

-- So this was basically cheating :P
-- No need to reconstruct full image, since corner pieces are easily detectable
isCornerPiece b borders = map (\s -> neighbourCands s bs') bsym
    where bs'   = deleteBy (\b b' -> (getId b) == (getId b')) b borders
          bsym  = symmetries b

main = do
    content <- readFile "testinput.txt"
    parsed <- return $ parse imgsParser "" content
    imgs <- case parsed of
              (Right x) -> return $ x
              (Left x) -> putStrLn (show x) >> error "Parse Error"
    gridSize <- return $ (floor . sqrt . fromIntegral . length) imgs
    borders <- return $ map extractBorders imgs
    cp <- return $ borders!!1
    putStrLn $ show cp
    return $ length $ isCornerPiece cp borders
    
