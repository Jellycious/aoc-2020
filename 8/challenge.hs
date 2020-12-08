import Text.Parsec as P
import Control.Applicative
import Data.Either

data Instr = Nop Int | Acc Int | Jmp Int 
    deriving (Show)

testProgram = ["nop +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"]

programParser = ((parseNop P.<|> parseAcc P.<|> parseJmp) `sepEndBy` (string "\n"))
parseNum :: Parsec String () Int
parseNum = (digitfy) <$> parseSign <*> parseDigits

digitfy '-' num = read ('-':num) :: Int
digitfy '+' num = read (num) :: Int

parseDigits :: Parsec String () String
parseDigits = many1 digit

parseSign :: Parsec String () Char 
parseSign = char '+' P.<|> char '-'

parseNop = string "nop" *> space *> ((Nop) <$> parseNum)
parseAcc = string "acc" *> space *> ((Acc) <$> parseNum)
parseJmp = string "jmp" *> space *> ((Jmp) <$> parseNum)

-- Return acc once executing a instruction twice.
runProgram :: [Instr] -> Int
runProgram p = runp [] 0 0
    where runp ls pc acc 
            | (pc `elem` ls)    = acc
            | otherwise         = case p!!pc of
                                    (Nop x) -> runp (pc:ls) (pc+1) acc 
                                    (Acc x) -> runp (pc:ls) (pc+1) (acc+x)
                                    (Jmp x) -> runp (pc:ls) (pc+x) acc

main = do
    program <- (parse programParser "") <$> readFile "input.txt"
    return (runProgram $ fromRight [] program)

