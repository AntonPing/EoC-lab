module Parser(parseExpr) where
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex
import Data.Char
import qualified Data.Text as T
import Control.Monad ( void )

import Syntax
import Data.Functor


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart    = "#{"
    , Tok.commentEnd      = "}#"
    , Tok.commentLine     = "#"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedNames   = [
        "=","->","=>","fn","let","in",
        "match", "of", "|",
        "if","then","else",
        "true", "false"
    ]
    , Tok.reservedOpNames = []
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

litInt :: Parser Int
litInt = fromIntegral <$> Tok.integer lexer

litReal :: Parser Double
litReal = Tok.float lexer

litBool :: Parser Bool
litBool = reserved "true" $> True
    <|> reserved "false" $> False

litChar :: Parser Char
litChar = Tok.charLiteral lexer

litString :: Parser String
litString = Tok.stringLiteral lexer

literal :: Parser Literal
literal = do
        LInt <$> litInt
    <|> LReal <$> litReal
    <|> LBool <$> litBool
    <|> LChar <$> litChar
    <?> "literal"

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

peek :: Parser a -> Parser b -> Parser b
peek p1 p2 = (try . lookAhead) p1 >> p2

varName :: Parser Name
varName = fmap T.pack (Tok.identifier lexer)

spaces1 :: Parser ()
spaces1 = skipMany1 space

variable :: Parser (Expr Name)
variable = EVar <$> varName

lambda :: Parser (Expr Name)
lambda = do
    reserved "fn" <?> "token \"fn\""
    args <- many1 varName <?> "arg!"
    reserved "->" <?> "arrow \"->\""
    body <- application
    return $ ELam args body


application :: Parser (Expr Name)
application = do
    xs <- sepBy1 expression spaces
    return $ EApp xs

letIn :: Parser (Expr Name)
letIn = do
    reserved "let"
    var <- varName <?> "var!"
    reserved "="
    def <- application <?> "app!"
    reserved "in"
    body <- application
    return $ ELet var def body

expression :: Parser (Expr Name)
expression = do
        peek (reserved "fn") lambda
    <|> peek (reserved "let") letIn
    <|> peek (char '(') (parens application)
    -- <|> peek (reserved "match") case'
    <|> try variable
    <|> ELit <$> literal
    <?> "Can't Parse Expression!"


tupled :: Parser a -> Parser [a]
tupled p = parens $ sepBy p (char ',')

{-

pVar :: Parser Pattern
pVar = PVar <$> varName
{-
pCon :: Parser Pattern
pCon = parens $ do
    x <- varName
    xs <- sepBy1 pattern' spaces
    return $ PCon x xs
-}
pTup :: Parser Pattern
pTup = do
    xs <- tupled pattern'
    return $ PTup xs

pLit :: Parser Pattern
pLit = PLit <$> literal

pWild :: Parser Pattern
pWild = char '_' >> return PWild

pattern' :: Parser Pattern
pattern' = choice [{-pCon,-}pTup,pVar,pLit,pWild]

branch :: Parser (Pattern,Expr)
branch = do
    reserved "|"
    pat <- pattern'
    spaces >> reserved "=>" >> spaces
    body <- expression
    return (pat,body)

case' :: Parser Expr
case' = do
    reserved "match"
    expr <- expression
    reserved "of"
    cases <- many1 branch
    return $ ECase expr cases
-}


parseExpr :: String -> Either ParseError (Expr Name)
parseExpr = parse expression "input"