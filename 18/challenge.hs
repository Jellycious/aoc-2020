import Text.Parsec
import Text.Parsec.Expr

data Expr = Mult Expr Expr | Add Expr Expr | C Int | P Expr
    deriving Show

-- PARSING
removeSpaces [] = []
removeSpaces (x:xs)
    | (x==' ')  = removeSpaces xs
    | otherwise = (x:removeSpaces xs)

numParser :: Parsec String () Int
numParser = read <$> (many1 digit)

constParser = C <$> numParser

parens = between (char '(') (char ')')

termParser = (P <$> parens exprParser) <|> constParser
exprParser = termParser `chainl1` binOp
binOp = do {char '*'; return Mult} <|> do {char '+'; return Add}

parseExpressions = parse (exprParser `endBy` newline) ""
-- END OF PARSING

evalExpr (Mult e1 e2) = (evalExpr e1) * (evalExpr e2)
evalExpr (Add e1 e2) = (evalExpr e1) + (evalExpr e2)
evalExpr (P e) = evalExpr e
evalExpr (C x) = x


main = do 
    content <- removeSpaces <$> readFile "input.txt"
    putStrLn content
    parsed <- return $ parseExpressions content
    p <- case parsed of
            (Right x) -> return $ x
            (Left x) -> do 
                            putStrLn (show x)
                            error "Couldn't parse input"
    return $ sum $ map evalExpr p
