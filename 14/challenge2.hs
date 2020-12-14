import Text.Parsec
import Data.Either
import Data.Map
import Data.Bits
import qualified Data.List as L
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
                                    (Mem d v) -> executeProgram is mask m'
                                        where as = applyMask mask d
                                              m' = Prelude.foldl (\m k -> insert k v m) mem as     

applyMask :: Instr -> Natural -> [Natural] 
applyMask (Mem m a) addr = error "Expecting Mask type Instr"
applyMask (Mask m) addr = applyM m 35 [addr]
            where applyM [] x as = as 
                  applyM (B0:mbits) x as = applyM mbits (x-1) as
                  applyM (B1:mbits) x as = applyM mbits (x-1) (L.map (flip setBit x) as)
                  applyM (X:mbits) x as = applyM mbits (x-1) (L.concatMap (\a -> [setBit a x, clearBit a x]) as)

sumMemory = Data.Map.foldr (+) 0

main = do
    content <- readFile "input.txt"
    parsed <- return $ parse programParser "" content
    program <- case parsed of 
        (Right x) -> return x
        (Left x) -> error $ show x 
    return $ sumMemory $ executeProgram program (Mask []) (Data.Map.empty) 
