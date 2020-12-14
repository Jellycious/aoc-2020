import Text.Parsec
import Data.Either
import Data.Map
import Data.Bits
import Numeric.Natural

data Instr = Mask [MBit] | Mem Natural Natural
    deriving (Show)
data MBit = X | B1 | B0
    deriving (Show)

maskParser :: Parsec String () Instr
maskParser = string "mask = " *> (Mask <$> many1 mBitParser)
mBitParser :: Parsec String () MBit
mBitParser = const X <$> char 'X' <|> const B1 <$> char '1' <|> const B0 <$> char '0'
memParser :: Parsec String () Instr
memParser = string "mem[" *> ((\dst val -> Mem (read dst :: Natural) (read val :: Natural)) <$> many1 digit) <*> (string "] = " *> many1 digit)
instrParser :: Parsec String () Instr
instrParser = try memParser <|> maskParser
programParser = instrParser `sepEndBy` (string "\n")
-- END OF PARSING

executeProgram [] mask mem = mem
executeProgram (i:is) mask mem = case i of
                                    (Mask m) -> executeProgram is (Mask m) mem
                                    (Mem d v) -> executeProgram is mask (insert d (applyMask mask v) mem)
applyMask (Mask mbits) val = applyM mbits 35 val
    where applyM [] x val = val
          applyM (X:mbits) x val = applyM mbits (x-1) val
          applyM (B1:mbits) x val = applyM mbits (x-1) (setBit val x)
          applyM (B0:mbits) x val = applyM mbits (x-1) (clearBit val x)

sumMemory = Data.Map.foldr (+) 0

testMask = fromRight (Mask []) $ parse maskParser "" "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse programParser "" content
    program <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    return $ sumMemory $ executeProgram program (Mask []) (Data.Map.empty) 
