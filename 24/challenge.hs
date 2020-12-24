import Text.Parsec
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

flipT Bl = Wh
flipT Wh = Bl 

flipTile dirs m = M.insert p col m
    where f [] pos = pos
          f (d:ds) pos = f ds (move d pos)
          p = f dirs (0,0)
          col = if M.member p m then flipT (m M.! p) else Bl

main = do 
    content <- readFile "input.txt"
    parsed <- return $ parse parseDirs "" content
    p <- case parsed of
           (Right x) -> return x
           (Left x) -> error "Parse Error"
    m <- return $ foldl (\m dirs -> flipTile dirs m) M.empty p 
    return $ M.size $ M.filter (==Bl) m
