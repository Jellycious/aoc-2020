import Text.Parsec
import Data.Foldable
import qualified Data.Map as M

-- PARSING
pE = E <$ string "e"
pW = W <$ string "w"
pSE = SE <$ string "se"
pSW = SW <$ string "sw"
pNE = NE <$ string "ne"
pNW = NW <$ string "nw"

parseDir = ((try pNE) <|> pNW) <|> ((try pSE) <|> pSW) <|> pE <|> pW
parseDirs = (many1 parseDir) `endBy` newline

-- EO PARSING
data T = Bl | Wh
    deriving(Show,Eq)

data Dir = E | W | SE | SW | NE | NW
    deriving(Show,Eq)

move E (x,y) = (x+2,y)
move W (x,y) = (x-2,y)
move SE (x,y) = (x+1,y+1)
move SW (x,y) = (x-1,y+1)
move NE (x,y) = (x+1,y-1)
move NW (x,y) = (x-1,y-1)

getTile p m = if M.member p m then m M.! p else Wh
addPos (x,y) (dx,dy) = (x+dx,y+dy)

flipT Bl = Wh
flipT Wh = Bl 

flipTile dirs m = M.insert p col m
    where f [] pos = pos
          f (d:ds) pos = f ds (move d pos)
          p = f dirs (0,0)
          col = if M.member p m then flipT (m M.! p) else Bl

niter = [(2,0),(-2,0),(1,1),(-1,1),(1,-1),(-1,-1)]
npos pos = map (addPos pos) niter
getNeighbours pos m = map (\p -> getTile (addPos pos p) m) niter

newTileCol pos m = if getTile pos m == Bl then cbl else cwh
    where bns = filter (==Bl) (getNeighbours pos m)
          cwh = if length bns == 2 then Bl else Wh 
          cbl = if (length bns == 1 || length bns == 2) then Bl else Wh

changeAllTiles m = M.mapWithKey (\pos col -> newTileCol pos m') m'
    where m' = expandGrid m
countBlackTiles m = M.size $ M.filter (==Bl) m

expandGridAtPos pos m = foldl (\m' p -> M.insert p (getTile p m) m') m (npos pos)
expandGrid m = foldl (\m k -> expandGridAtPos k m) m (M.keys (M.filter (==Bl) m))









main = do 
    content <- readFile "input.txt"
    parsed <- return $ parse parseDirs "" content
    p <- case parsed of
           (Right x) -> return x
           (Left x) -> error "Parse Error"
    m <- return $ foldl (\m dirs -> flipTile dirs m) M.empty p 
    putStrLn $ show $ countBlackTiles $ foldl' (\m' i -> changeAllTiles m') m [1..100]
    return 0
