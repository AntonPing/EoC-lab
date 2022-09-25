{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}


module ParserNew(parseExpr) where
import Text.Parsec as P
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
        "=", "->", "=>",
        "fn", "let", "letrec", "in", "end",
        "match", "case", "of", "|",
        "if","then","else",
        "true", "false","()",
        "iadd","isub","ineg",
        "cmpeq","cmpne","cmpgr","cmpls","cmpge","cmple",
        "band","bor","bxor","bnot",
        "read-int", "write-int"
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

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

spaces1 :: Parser ()
spaces1 = skipMany1 space

peek :: Parser a -> Parser b -> Parser b
peek p1 p2 = (try . lookAhead) p1 >> p2

litInt :: Parser Int
litInt = fromIntegral <$> Tok.integer lexer

litReal :: Parser Double
litReal = Tok.float lexer

litBool :: Parser Bool
litBool = reserved "true" $> True
    <|> reserved "false" $> False

litChar :: Parser Char
litChar = Tok.charLiteral lexer

litUnit :: Parser ()
litUnit = reserved "()"

--litString :: Parser String
--litString = Tok.stringLiteral lexer

literal :: Parser Literal
literal = LInt <$> litInt
    <|> LReal <$> litReal
    <|> LBool <$> litBool
    <|> LChar <$> litChar
    <|> LUnit <$ litUnit
    <?> "literal"

primitive :: Parser Prim
primitive = IAdd <$ reserved "iadd"
    <|> ISub <$ reserved "isub"
    <|> INeg <$ reserved "ineg"
    <|> ICmp Eq <$ reserved "cmpeq"
    <|> ICmp Ne <$ reserved "cmpne"
    <|> ICmp Gr <$ reserved "cmpgr"
    <|> ICmp Ls <$ reserved "cmpls"
    <|> ICmp Ge <$ reserved "cmpge"
    <|> ICmp Le <$ reserved "cmple"
    <|> BAnd <$ reserved "band"
    <|> BOr <$ reserved "bor"
    <|> BNot <$ reserved "bnot"
    <|> BNot <$ reserved "bxor"


name :: Parser Name
name = Tok.identifier lexer

exprNoApp :: Parser (Expr Name)
exprNoApp = choice 
    [ eLit, eVar, eFun, eOpr, eLet, eFix, eIfte, parens expr ]

expr :: Parser (Expr Name)
expr = do
    func <- exprNoApp
    argss <- many $ tupled expr
    return $ foldl EApp func argss

eLit :: Parser (Expr Name)
eLit = ELit <$> literal

eVar :: Parser (Expr Name)
eVar = EVar <$> name

eFun :: Parser (Expr Name)
eFun = do
    reserved "fn" <?> "token \"fn\""
    args <- tupled name <?> "parameter list"
    reserved "=>" <?> "token \"->\""
    body <- expr <?> "function body"
    return $ EFun args body

tupled :: Parser a -> Parser [a]
tupled p = parens $ sepBy p (char ',')

-- backtracking is bad, this is never used.
eApp :: Parser (Expr Name)
eApp = do
    func <- expr
    args <- tupled expr
    return $ EApp func args

eOpr :: Parser (Expr Name)
eOpr = do
    prim <- primitive
    args <- tupled expr
    return $ EOpr prim args

eLet :: Parser (Expr Name)
eLet = do
    reserved "let"
    name <- name
    reserved "="
    body <- expr
    reserved ";"
    cont <- expr
    return $ ELet name body cont

def :: Parser (Def Name)
def = do
    func <- name <?> "function name"
    args <- tupled name <?> "parameter list"
    reserved "="
    body <- expr <?> "function body"
    reserved ";"
    return $ Def { func, args, body }

eFix :: Parser (Expr Name)
eFix = do
    reserved "letrec"
    defs <- many1 def
    reserved "in"
    cont <- expr
    reserved "end"
    return $ EFix defs cont

eIfte :: Parser (Expr Name)
eIfte = do
    reserved "if"
    cond <- expr
    reserved "then"
    trbr <- expr
    reserved "else"
    flbr <- expr
    return $ EIfte cond trbr flbr

{-

pVar :: Parser Pattern
pVar = PVar <$> name
{-
pCon :: Parser Pattern
pCon = parens $ do
    x <- name
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
    body <- expr
    return (pat,body)

case' :: Parser Expr
case' = do
    reserved "match"
    expr <- expr
    reserved "of"
    cases <- many1 branch
    return $ ECase expr cases
-}


parseExpr :: String -> Either ParseError (Expr Name)
parseExpr = parse expr "input"