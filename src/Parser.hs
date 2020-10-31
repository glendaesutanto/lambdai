module Parser (lambdaRead)
where

import Data.Char
import Text.ParserCombinators.Parsec

import LambdaAST

definition :: Parser LambdaTerm
definition = do
  l <- variable
  spaces
  char '='
  spaces
  t <- lambdaTerm
  return $ Definition (var l) t

variable :: Parser LambdaTerm
variable =
  do var <- many1 $ satisfy (\c -> not (isSpace c) && not (elem c "\\λ.()="))
     return $ Variable var

application :: Parser LambdaTerm
application =
  chainl1 lambdaTermNoApp (spaces >> return Application)

lambdaAbstraction :: Parser LambdaTerm
lambdaAbstraction =
  do char 'λ' <|> char '\\'
     x <- variable
     char '.'
     spaces
     term <- lambdaTerm
     return $ Lambda (var x) term

topLevelLambdaTerm :: Parser LambdaTerm
topLevelLambdaTerm =   (try definition)
                   <|> lambdaTerm

lambdaTerm :: Parser LambdaTerm
lambdaTerm =   (try application)
           <|> lambdaTermNoApp

lambdaTermNoApp :: Parser LambdaTerm
lambdaTermNoApp =   (char '(' >> lambdaTerm >>= (\lt -> char ')' >> return lt))
                <|> lambdaAbstraction
                <|> variable

lambdaRead :: String -> Either ParseError LambdaTerm
lambdaRead s = parse (topLevelLambdaTerm >>= (\term -> eof >> return term)) "" $ process s

process :: [Char] -> [Char]
process s = concat $ map (\x -> convert x) s

convert :: Char -> [Char]
convert x
    | isNumber x = toLambdaExpression (digitToInt x)
    | x == '+' = "(\\w.(\\y.(\\x.(y(w y x)))))"
    | x == '*' = "(\\x.(\\y.(\\z.(x(y z)))))"
    | otherwise = [x]

toLambdaExpression :: Int -> [Char]
toLambdaExpression n = "(\\s.(\\z." ++ toLambdaExpressionHelper n ++ "))"

toLambdaExpressionHelper :: Int -> [Char]
toLambdaExpressionHelper 0 = "z"
toLambdaExpressionHelper 1 = "(s z)"
toLambdaExpressionHelper n = "(s" ++ toLambdaExpressionHelper (n-1) ++ ")"
