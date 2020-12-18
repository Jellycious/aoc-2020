import Text.Parsec
import Text.Parsec.Expr

-- PARSING
removeSpaces [] = []
removeSpaces (x:xs)
    | (x==' ')  = removeSpaces xs
    | otherwise = (x:removeSpaces xs)

numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

expr = chainl1 term multop
term = chainl1 factor addop
factor = parens expr <|> numParser

addop = do { char '+'; return (+)}
multop = do { char '*'; return (*)}

parens = between (char '(') (char ')')

parseExpressions = parse (expr `endBy` newline) ""
--End of Parsing


main = do 
    content <- removeSpaces <$> readFile "input.txt"
    parsed <- return $ parseExpressions content
    p <- case parsed of
            (Right x) -> return $ x
            (Left x) -> do 
                            putStrLn (show x)
                            error "Couldn't parse input"
    return $ sum p
