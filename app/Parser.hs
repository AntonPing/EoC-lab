{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}


module Parser(parseExpr) where
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
        "fn", "let", "letrec", "in",
        "match", "case", "of", "|",
        "if","then","else",
        "true", "false","()",
        "iadd","isub","ineg",
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

prim :: Parser Prim
prim = IAdd <$ reserved "iadd"
    <|> ISub <$ reserved "isub"
    <|> INeg <$ reserved "ineg"
    <|> INeg <$ reserved "ineg"

varName :: Parser Name
varName = Tok.identifier lexer

eLit :: Parser (Expr Name)
eLit = ELit <$> literal

eVar :: Parser (Expr Name)
eVar = EVar <$> varName

eFun :: Parser (Expr Name)
eFun = do
    reserved "fn" <?> "token \"fn\""
    args <- many1 varName <?> "parameter list"
    reserved "=>" <?> "token \"->\""
    body <- eAppOpr <?> "function body"
    return $ EFun args body


-- kind of confusing, 'cause it doesn't always returns applaction
-- for example: (f x y) will return EApp f [EVar x, EVar y]
-- and (f x) will return EApp f []
-- while (f) will return EVar f
eApp :: Parser (Expr Name)
eApp = parens eAppOpr

tupled :: Parser a -> Parser [a]
tupled p = parens $ sepBy p (char ',')

def :: Parser (Def Name)
def = do
    func <- varName <?> "parameter list"
    args <- tupled varName
    reserved "="
    body <- expr <?> "function body"
    return $ Def { func, args, body }

eLet :: Parser (Expr Name)
eLet = do
    reserved "let"
    name <- varName
    reserved "="
    body <- expr
    reserved ";"
    cont <- expr
    return $ ELet name body cont

eFix :: Parser (Expr Name)
eFix = do
    reserved "letrec"
    defs <- sepBy1 def (reserved ";")
    reserved "in"
    cont <- expr
    return $ EFix defs cont

-- returns either EApp or EOpr
eAppOpr :: Parser (Expr Name)
eAppOpr = parens $ do
    mayprim <- optionMaybe prim
    xs <- sepBy1 expr spaces
    case (mayprim,xs) of
        (Just prim, args) ->
            if arity prim == length args
            then return $ EOpr prim args
            else fail "arity doesn't match"
        (Nothing, func:args) ->
            return $ EApp func args
        (Nothing, []) ->
            -- this should never happen, 'cause "()" is a keyword for unit
            fail "application without function, what?"

expr :: Parser (Expr Name)
expr = choice 
    [ eAppOpr, eLit, eVar, eFun, eLet, eFix ]



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