import Text.Parsec

data Grid = G [[[[Bool]]]]

-- Nice Debug Function
instance Show Grid where
    show (G g) = concat $ map (\wi -> concat $ map (\zi -> printSlice (wi,zi,g!!(wi+w)!!(zi+z))) [-z..z]) [-w..w]
        where w = ((length g)-1) `div` 2
              z = ((length (g!!0))-1) `div` 2
              printSlice (wi,zi,s) = "z="++(show zi)++" and w="++(show wi)++"\n"++(printRows s)++"\n"
              printRow r = map (\x -> if x then '#' else '.') r ++ "\n"
              printRows c = concat $ map (printRow) c

parseRow = many1 ((const False) <$> char '.' <|> (const True) <$> char '#')
parseRows = (\c -> G [[c]]) <$> parseRow `endBy` newline
-- End of Parsing

-- Expands grid in all directions
expandGrid :: Grid -> Grid
--expandGrid (G grid) = G $ expandZ (map (\c -> expandY (map expandX c)) grid)
expandGrid (G grid) = G $ expandW (map (\s -> expandZ (map (\c -> expandY (map expandX c)) s)) grid)
    where xl = length ((grid!!0)!!0!!0)+2
          yl = length (grid!!0!!0)+2
          zl = length (grid!!0)+2
          expandX r = [False]++r++[False]
          expandY c = [(take xl $ repeat False)]++c++[(take xl $ repeat False)]
          expandZ s = [(take yl $ repeat (take xl $ repeat False))]++s++[(take yl $ repeat (take xl $ repeat False))]
          expandW h = [(take zl $ repeat (take yl $ repeat (take xl $ repeat False)))]++h++[(take zl $ repeat (take yl $ repeat (take xl $ repeat False)))]

getPos (G g) (-1) z y x = [] 
getPos (G g) w (-1) y x = [] 
getPos (G g) w z (-1) x = []
getPos (G g) w z y (-1) = []
getPos (G g) w z y x = if tooBig then [] else [g!!w!!z!!y!!x]
    where tooBig = w >= length g || z >= length (g!!0) || y >= length ((g!!0)!!0) || x >= length (((g!!0)!!0)!!0)

d = [(w,z,y,x) | w <- [-1..1], z <- [-1..1], y <- [-1..1], x <- [-1..1], (w,z,y,x) /= (0,0,0,0)]

getNeighbours g (w, z, y, x) = concat $ map (\(dw,dz,dy,dx) -> getPos g (w+dw) (z+dz) (y+dy) (x+dx)) d
activeNeighbours g pos = filter (==True) (getNeighbours g pos)
countActiveCubes (G g)  = sum $ map (\s -> sum $ map (\c -> sum $ map (\r -> length $ filter (==True) r) c) s) g 

coordsGen (G g) = map (\w -> map (\z -> map (\y -> map (\x -> (w,z,y,x)) [0..xl]) [0..yl]) [0..zl]) [0..wl]
    where xl = (length (g!!0!!0!!0)) - 1
          yl = (length (g!!0!!0)) - 1
          zl = (length (g!!0)) - 1
          wl = (length (g)) - 1

updateCube g (w,z,y,x) = if active then (if nc >= 2 && nc <= 3 then True else False) else (if nc == 3 then True else False)
    where active = head $ getPos g w z y x -- potentially throws error. (when we want an error)
          nc = length $ activeNeighbours g (w,z,y,x)

--gridIter (G g) = map (\(c, ci) -> map (\(r, ri) -> map (\(x, xi) -> ((ci,ri,xi), x)) (zip r [0..xl]) ) (zip c [0..yl])) (zip g [0..zl])
--    where xl = (length ((g!!0)!!0)) - 1
--          yl = (length (g!!0)) - 1
--          zl = (length (g)) - 1

updateGrid :: Grid -> Grid
updateGrid g = G $ map (map (map (map (updateCube gex)))) git
    where gex = expandGrid g
          git = coordsGen gex

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse parseRows "" content
    (G p) <- case parsed of
            (Right x) -> return $ x
            (Left x) -> return $ error "Parse Error"
    cnt <- return $ countActiveCubes $ foldl (\g _ -> updateGrid g) (G p) [1..6]
    putStrLn $ show $ cnt
    return cnt
