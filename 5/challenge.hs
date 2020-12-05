testSeat = "FBFBBFFRLR" 

uid (row, col) = row * 8 + col

seatRow seat = take 7 seat
seatCol seat = drop 7 seat

rowOptions = [0..127]
colOptions = [0..7]

findcol sc = fc sc colOptions
    where   fc [] opt = opt!!0 
            fc ('R':xs) opt = fc xs $ drop ((length opt) `div` 2) opt
            fc ('L':xs) opt = fc xs $ take ((length opt) `div` 2) opt

findrow sc = fc sc rowOptions
    where   fc [] opt = opt!!0 
            fc ('B':xs) opt = fc xs $ drop ((length opt) `div` 2) opt
            fc ('F':xs) opt = fc xs $ take ((length opt) `div` 2) opt

computeUid seat = uid s
    where s = ((findrow $ seatRow seat), (findcol $ seatCol seat))


main = do
    seats <- lines <$> readFile "input.txt"
    sol <- pure $ computeUid <$> seats
    return sol
