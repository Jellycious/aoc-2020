import Text.Parsec
import Data.List
import qualified Data.Map as M
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
testParser = parse imgParser "" testInput

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

data Sym = Id | R1 | R2 | R3 | TX | TY | T13 | T24
    deriving (Show, Eq)

getGrid (Img k g) = g
prettyShow g = show $ Img 0 g

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

removeBorders :: [[Tile]] -> [[Tile]]
removeBorders g = map (init . tail) ((init . tail) g)

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

applySym Id = id
applySym TX = reverse
applySym TY = map reverse
applySym R1 = reverse . transpose 
applySym R2 = (applySym R1) . (applySym R1)
applySym R3 = (applySym R1) . (applySym R1) . (applySym R1)
applySym T13 = transpose
applySym T24 = (applySym R1) . reverse

symmetries b = (b:(tX b):(tY b):(r1 b):(r2 b):(r3 b):(t13 b):(t24 b):[])
symmetries2 b = ((b, Id):((tX b), TX):((tY b), TY):((r1 b),R1):((r2 b),R2):((r3 b),R3):((t13 b),T13):((t24 b),T24):[])
symmetriesList = [Id, TX, TY, R1, R2, R3, T13, T24]


-- Returns possible orientations of b which aligns with fixed top image bt
alignsT bt b = filter pred (symmetries2 b)
    where pred (b,s) = sameBorder (getBottom bt) (getTop b)
alignsB bb b = filter pred (symmetries2 b)
    where pred (b,s) = sameBorder (getTop bb) (getBottom b)
alignsR br b = filter pred (symmetries2 b)
    where pred (b,s) = sameBorder (getLeft br) (getRight b)
alignsL bl b = filter pred (symmetries2 b)
    where pred (b,s) = sameBorder (getRight bl) (getLeft b)

-- Finds the elements in borders which could align with fixed element b
-- for alignFunc == AlignsT it would find the bottom neighbours or in other words, the elements which can have b on top.
-- borders indicates the candidate borders
findAligns alignFunc b borders = concatMap (alignFunc b) borders

neighbourCands b borders = (tns, rns, bns, lns)
    where tns = findAligns alignsB b borders -- Possible top neighbours
          rns = findAligns alignsL b borders
          bns = findAligns alignsT b borders
          lns = findAligns alignsR b borders

isCornerPiece b borders = (length $ filter (\(tns, rns, bns, lns) -> tns == [] && lns == []) $ map (\s -> neighbourCands s borders) bsym) > 0
    where bsym  = symmetries b

deleteB :: Borders -> [Borders] -> [Borders]
deleteB b borders = deleteBy (\b b' -> (getId b) == (getId b')) b borders
-- Construct the actual image.
diagonalIter :: Int -> [(Int, Int)]
diagonalIter size = [(x,y) | sum <- [0..(size*2-1)], x <- [0..(size-1)], y <- [0..(size-1)], x+y==sum]

-- Find possible candidates for position: pos.
candidates :: M.Map (Int,Int) (Borders, Sym) -> (Int, Int) -> [Borders] -> [(Borders, Sym)]
candidates map (0,x) borders = findAligns alignsL b borders 
    where (b, s) = map M.! (0,x-1)
candidates map (y,0) borders = findAligns alignsT b borders 
    where (b, s) = map M.! (y-1,0)
candidates map (y,x) borders = intersect la ta
    where (bt, st) = map M.! (y-1,x)
          (bl, sl) = map M.! (y,x-1)
          la = findAligns alignsL bl borders
          ta = findAligns alignsT bt borders

-- Constructs the grid given a map with an initial top-left corner piece
reassemble map [] borders = return map
reassemble map (pos:poss) borders = do
    (cand, sym) <- candidates map pos borders
    reassemble (M.insert pos (cand, sym) map) poss (deleteB cand borders)

solve :: [Borders] -> [Borders] -> Int -> [M.Map (Int, Int) (Borders, Sym)]
solve cornerPieces borders gridSize = concatMap (\(p, s) -> reassemble (M.singleton (0,0) (p, s)) (drop 1 $ diagonalIter gridSize) (deleteB p borders)) cornerPiecesSym
    where cornerPiecesSym = concatMap symmetries2 cornerPieces

createImgMap :: [Img] -> M.Map Int [[Tile]]
createImgMap imgs = foldl (\m (Img k v) -> M.insert k v m) M.empty imgs

-- Construct a single grid from small grids
combineGridX grid1 grid2 = map (\(r,r') -> r++r') (zip grid1 grid2)
combineGridY grid1 grid2 = grid1 ++ grid2

combineAlongIterHorizontally imgmap posmap (k:[]) = grid
    where (b, sym) = posmap M.! k
          grid = (applySym sym) $ (imgmap M.! (getId b))
combineAlongIterHorizontally imgmap posmap (k:ks) = combineGridX grid $ combineAlongIterHorizontally imgmap posmap ks
    where (b, sym) = posmap M.! k
          grid = (applySym sym) $ (imgmap M.! (getId b))

combineAlongIterVertically imgmap posmap (k:[]) = grid
    where (b, sym) = posmap M.! k
          grid = (applySym sym) $ (imgmap M.! (getId b))
combineAlongIterVertically imgmap posmap (k:ks) = combineGridY grid $ combineAlongIterVertically imgmap posmap ks
    where (b, sym) = posmap M.! k
          grid = (applySym sym) $ (imgmap M.! (getId b))

xIter gridSize y = [(y,x) | x <- [0..(gridSize - 1)]]
reconstructGrid gridSize imgmap posmap = foldl (++) [] (map (\y -> combineAlongIterHorizontally imgmap posmap (xIter gridSize y)) [0..(gridSize - 1)])

-- Count sea monster in big grid
monsterPattern = [(0,18),(1,0),(1,5),(1,6),(1,11),(1,12),(1,17),(1,18),(1,19),(2,1),(2,4),(2,7),(2,10),(2,13),(2,16)] :: [(Int,Int)]
shiftPattern dx = [(y,x+dx) | (y,x) <- monsterPattern]
testPattern rows dx = foldl (&&) True ((map (\(y,x) -> (rows!!y!!x)==X)) (shiftPattern dx))

countMonsters (r1:r2:[]) = 0
countMonsters rs@(r1:r2:r3:rows) = (length $ filter (==True) $ map (\s -> testPattern rs s) [0..shifts]) + countMonsters (r2:r3:rows)
    where rowLength = length r1
          shifts = rowLength - 20

countX grid = sum $ map (\r -> sum $ map (\x -> if x == X then 1 else 0) r) grid


test = do
    content <- readFile "testinput.txt"
    parsed <- return $ parse imgsParser "" content
    imgs <- case parsed of
              (Right x) -> return $ x
              (Left x) -> putStrLn (show x) >> error "Parse Error"
    imgmap <- return $ M.map (removeBorders) $ createImgMap imgs
    gridSize <- return $ (floor . sqrt . fromIntegral . length) imgs
    borders <- return $ map extractBorders imgs

    b1951 <- return $ borders!!1
    b2311 <- return $ borders!!0
    b2729 <- return $ borders!!7
    b1171 <- return $ borders!!2
    solution <- return $ head $ solve [b1951, b2311, b2729, b1171] borders gridSize

    grid <- return $ reconstructGrid gridSize imgmap solution 
    mcount <- return $ maximum $ map (\sym -> countMonsters (applySym sym grid)) symmetriesList
    xcount <- return $ countX grid 
    return (xcount - (mcount * (length monsterPattern))) 


-- Corner pieces are 1049, 2081, 2129, 3229
-- This can be computed using the solution to the first-part.
main = do
    content <- readFile "input.txt"
    parsed <- return $ parse imgsParser "" content
    imgs <- case parsed of
              (Right x) -> return $ x
              (Left x) -> putStrLn (show x) >> error "Parse Error"
    imgmap <- return $ M.map (removeBorders) $ createImgMap imgs
    gridSize <- return $ (floor . sqrt . fromIntegral . length) imgs
    borders <- return $ map extractBorders imgs

    b1049 <- return $ borders!!62 -- corner piece
    b2081 <- return $ borders!!93 -- corner piece
    b2129 <- return $ borders!!118 -- corner piece
    b3229 <- return $ borders!!124 -- corner piece
    solution <- return $ head $ solve [b1049, b2081, b2129, b3229] borders gridSize

    grid <- return $ reconstructGrid gridSize imgmap solution 
    mcount <- return $ maximum $ map (\sym -> countMonsters (applySym sym grid)) symmetriesList
    xcount <- return $ countX grid 
    return (xcount - (mcount * (length monsterPattern))) 
    
