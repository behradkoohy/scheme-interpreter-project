import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex


data LispVal =    Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                | Character Char
                | Float Double
                | Ratio Rational
                | Complex (Complex (Double))

instance Show LispVal where show = showVal


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?" , unaryOp listp),
              ("symbol->string", unaryOp symbToStr),
              ("string->symbol", unaryOp strToSymb)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [x] = f x 

symbToStr :: LispVal -> LispVal
symbToStr (Atom s) = (String s)
symbToStr _ = (String "")

strToSymb :: LispVal -> LispVal
strToSymb (String s) = (Atom s)
strToSymb _ = (String "")

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False

boolp :: LispVal -> LispVal
boolp   (Bool _)   = Bool True
boolp   _          = Bool False

listp :: LispVal -> LispVal
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool False
listp   _          = Bool False

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
-- unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
--                            if null parsed 
--                               then 0
--                               else fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0


spaces :: Parser ()
spaces = skipMany1 space 

escapedChars :: Parser Char
escapedChars = do 
                char '\\'
                x <- oneOf "\\\"nrt"
                return $ case x of
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'

parseChar :: Parser LispVal
parseChar = do 
                try $ string "#\\"
                value <- try (string "newline" <|> string "space") <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                return (case value of
                    "space" -> Character ' '
                    "newline" -> Character '\n'
                    otherwise -> Character (value !! 0)
                    )


parseFloat :: Parser LispVal
parseFloat = do 
                x <- many1 digit
                char '.'
                y <- many1 digit
                return (Float ( fst (head  (readFloat (x++"."++y)))))

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapedChars <|> noneOf "\"")
                char '"'
                return (String x)

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return (case atom of 
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _ -> Atom atom 
                    )

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return (DottedList head tail)

parseDecimal1 :: Parser LispVal
parseDecimal1 = do 
                num <- many1 digit
                let numx = read num :: Integer
                return (Number numx)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do 
                try $ string "#d"
                x <- many1 digit
                (return . Number . read) x

parseHex :: Parser LispVal 
parseHex = do 
            try $ string "#x"
            x <- many1 hexDigit
            return $ Number (hex2dig x)            

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


parseOct :: Parser LispVal 
parseOct = do 
            try $ string "#o"
            x <- many1 octDigit
            return $ Number (oct2dig x)


parseRatio :: Parser LispVal
parseRatio = do 
                nom <- many1 digit
                char '\\'
                denom <- many1 digit
                return (Ratio ((read nom) % (read denom)))


toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n


parseComplex :: Parser LispVal
parseComplex = do
                x <- try (parseFloat <|> parseDecimal1 <|> parseDecimal2)
                char '+'
                y <- try (parseFloat <|> parseDecimal1 <|> parseDecimal2)
                char 'i'
                return (Complex ((toDouble x) :+ (toDouble y)))


parseBin :: Parser LispVal 
parseBin = do 
            try $ string "#b"
            x <- many1 (oneOf "10")
            return $ Number (bin2dig x)


parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin


parseBool :: Parser LispVal
parseBool = do
    char '#'
    ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))


parseExpr :: Parser LispVal
parseExpr = parseAtom <|> 
            parseString <|> 
            try parseFloat <|> 
            try parseComplex <|> 
            try parseRatio <|> 
            try parseNumber <|> 
            try parseBool <|> 
            try parseChar <|> 
            parseQuoted <|> do 
                    char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x


oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (map eval args)


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ (show err)
    Right val -> val


apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives


showVal :: LispVal -> String
showVal (String x) = "\"" ++ x ++ "\""
showVal (Atom x) = x
showVal (Number x) =  show x
showVal (Bool x)    | x == True = "#t"
                    | x == False = "#f"
showVal (List x) = "(" ++ (unwordsList x) ++ ")"
showVal (DottedList head tail) = "(" ++ (unwordsList head) ++ (showVal tail) ++ ")"



main :: IO ()
main = getArgs >>= print . eval . readExpr . head
-- main = do 
--     (expr:_) <- getArgs
--     putStrLn (readExpr expr)