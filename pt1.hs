import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
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

data LispError =    NumArgs Integer [LispVal]
                   | TypeMismatch String LispVal
                   | Parser ParseError
                   | BadSpecialForm String LispVal
                   | NotFunction String String
                   | UnboundVar String String
                   | Default String

type ThrowsError = Either LispError

instance Show LispVal where show = showVal
instance Show LispError where show = showError


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
-- unaryOp f l@(x:_) = throwError $ NumArgs 1 l 



symbolp, numberp, stringp, boolp, listp, symbToStr, strToSymb :: LispVal -> ThrowsError LispVal
symbolp (Atom _) = return $ Bool True
symbolp _ = return $ Bool False

symbToStr (Atom s) = return (String s)
symbToStr s = throwError $ TypeMismatch "symbol" s

strToSymb (String s) = return (Atom s)
strToSymb s = throwError $ TypeMismatch "string" s

numberp (Number _) = return $ Bool True
numberp _          = return $ Bool False

stringp (String _) = return $ Bool True
stringp _          = return $ Bool False

boolp   (Bool _)   = return  $ Bool True
boolp   _          = return $ Bool False

listp   (List _)   = return $ Bool True
listp   (DottedList _ _) = return $ Bool True
listp   _          = return $ Bool False

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"



unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum
-- unpackNum :: LispVal -> Integer
-- unpackNum (Number n) = n
-- unpackNum _ = 0
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


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


showVal :: LispVal -> String
showVal (String x) = "\"" ++ x ++ "\""
showVal (Atom x) = x
showVal (Number x) =  show x
showVal (Bool x)    | x == True = "#t"
                    | x == False = "#f"
showVal (List x) = "(" ++ (unwordsList x) ++ ")"
showVal (DottedList head tail) = "(" ++ (unwordsList head) ++ (showVal tail) ++ ")"

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val





main :: IO ()
main = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled
-- main = getArgs >>= print . eval . readExpr . head
-- main = do 
--     (expr:_) <- getArgs
--     putStrLn (readExpr expr)