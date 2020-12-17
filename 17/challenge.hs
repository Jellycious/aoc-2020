import Text.Parsec

data Grid = G [[[Bool]]]

-- Nice Debug Function
instance Show Grid where
    show (G g) = concat $ map printSlice (zip [-z..] g)
        where z = ((length g)-1) `div` 2
              printSlice (i,s) = "z="++(show i)++":\n"++(printRows s)++"\n"
              printRow r = map (\x -> if x then '#' else '.') r ++ "\n"
              printRows c = concat $ map (printRow) c


parseRow = many1 ((const False) <$> char '.' <|> (const True) <$> char '#')
parseRows = (\c -> G [c]) <$> parseRow `endBy` newline
-- End of Parsing

-- Expands grid in all directions
expandGrid :: Grid -> Grid
expandGrid (G grid) = G $ expandZ (map (\c -> expandY (map expandX c)) grid)
    where xl = length ((grid!!0)!!0)+2
          yl = length (grid!!0)+2
          expandX r = [False]++r++[False]
          expandY c = [(take xl $ repeat False)]++c++[(take xl $ repeat False)]
          expandZ s = [(take yl $ repeat (take xl $ repeat False))]++s++[(take yl $ repeat (take xl $ repeat False))]

getPos (G g) (-1) y x = [] 
getPos (G g) z (-1) x = []
getPos (G g) z y (-1) = []
getPos (G g) z y x = if tooBig then [] else [g!!z!!y!!x]
    where tooBig = z >= length g || y >= length (g!!0) || x >= length ((g!!0)!!0)

d = [(z,y,x) | z <- [-1..1], y <- [-1..1], x <- [-1..1], (z,y,x) /= (0,0,0)]

getNeighbours g (z, y, x) = concat $ map (\(dz,dy,dx) -> getPos g (z+dz) (y+dy) (x+dx)) d
activeNeighbours g (z, y, x) = filter (==True) (getNeighbours g (z, y, x))
countActiveCubes (G g)  = sum $ map (\c -> sum $ map (\r -> length $ filter (==True) r) c) g

coordsGen (G g) = map (\z -> map (\y -> map (\x -> (z,y,x)) ([0..xl]))([0..yl])) [0..zl]
    where xl = (length ((g!!0)!!0)) - 1
          yl = (length (g!!0)) - 1
          zl = (length (g)) - 1

updateCube g (z,y,x) = if active then (if nc >= 2 && nc <= 3 then True else False) else (if nc == 3 then True else False)
    where active = head $ getPos g z y x -- potentially throws error. (when we want an error)
          nc = length $ activeNeighbours g (z,y,x)

--gridIter (G g) = map (\(c, ci) -> map (\(r, ri) -> map (\(x, xi) -> ((ci,ri,xi), x)) (zip r [0..xl]) ) (zip c [0..yl])) (zip g [0..zl])
--    where xl = (length ((g!!0)!!0)) - 1
--          yl = (length (g!!0)) - 1
--          zl = (length (g)) - 1

updateGrid :: Grid -> Grid
updateGrid g = G $ map (map (map (updateCube gex))) git
    where gex = expandGrid g
          git = coordsGen gex




main = do
    content <- readFile "input.txt"
    parsed <- return $ parse parseRows "" content
    p <- case parsed of
            (Right x) -> return $ x
            (Left x) -> return $ error "Parse Error"
    return $ countActiveCubes $ foldl (\g i -> updateGrid g) p [1..6]
    
