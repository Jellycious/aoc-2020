import Text.Parsec as P
import Control.Applicative
import Control.Exception
import Data.Either

data Instr = Nop Int | Acc Int | Jmp Int 
    deriving (Show)

data ParseException = ParseException
    deriving (Show)

instance Exception ParseException

testProgramInput = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"
testProgram = fromRight [] (parse programParser "" testProgramInput)


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

terminates :: [Instr] -> [Int] 
terminates p = runp [] 0 0 
    where runp ls pc acc 
            | (pc `elem` ls)    = ls    -- Last Instruction before loop
            | (pc >= (length p))= []    -- Program terminates 
            | otherwise         = case p!!pc of
                                    (Nop x) -> runp (pc:ls) (pc+1) acc 
                                    (Acc x) -> runp (pc:ls) (pc+1) (acc+x)
                                    (Jmp x) -> runp (pc:ls) (pc+x) acc
runProgram :: [Instr] -> Int
runProgram p = runp 0 0
    where runp pc acc 
            | (pc >= length p)  = acc
            | otherwise         = case p!!pc of
                                    (Nop x) -> runp (pc+1) acc 
                                    (Acc x) -> runp (pc+1) (acc+x)
                                    (Jmp x) -> runp (pc+x) acc

test :: [Instr] -> Bool
test p = length (terminates p) == 0

findcorrupt :: [Instr] -> Int
findcorrupt p = fc p (terminates p)
    where fc p [] = error "Could Not Solve"
          fc p (x:xs) = if test (flipInstrAt p x) then x else fc p xs

flipInstr :: Instr -> Instr
flipInstr (Nop x) = Jmp x
flipInstr (Jmp x) = Nop x
flipInstr (Acc x) = Acc x -- Doesn't change

flipInstrAt :: [Instr] -> Int -> [Instr]
flipInstrAt p pc = (take pc p)++[i]++(drop (pc+1) p)
    where i = flipInstr (p!!pc)

-- Second-Part solution
solve p = runProgram (flipInstrAt p sol)
    where sol = findcorrupt p

main = do
    program <- (parse programParser "") <$> readFile "input.txt"
    case program of 
        (Right a)   -> putStrLn (show $ solve $ fromRight [] program)
        _           -> throw (ParseException)  

