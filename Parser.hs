module Parser where

import           Data.Functor.Identity
import           MainLang
import           Prelude
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token
  --Takes a string and output an ITerm.
  --iTerms :: Parser ITerms

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames =
        [ "*"
        , "Nat"
        , "Zero" --ITerms without inputs
        , "Ann"
        , "Pi"
        , "Succ"
        , "NatElim" --ITerms with CTerms as inputs
        , "Vec"
        , "Nil"
        , "Cons"
        , "VecElim"
        , "Eq"
        , "Refl"
        , "EqElim"
        , "Bound" --Bound var
        , "Free"
        , "Local"
        , "Quote"
        , "Global" --Free var
        ]
    , Token.reservedOpNames =
        [ "Inf"
        , "Lam"
        , ":"
        , ":@:" --application
        ]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier

reserved :: String -> Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser a -> Parser a
parens = Token.parens lexer -- parses surrounding parenthesis:
                                      --   parens p
                                      -- takes care of the parenthesis and
                                      -- uses p to parse what's inside them

natural :: Parser Integer
natural = Token.natural lexer -- parses an integer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parseSimpleI :: (String, b) -> Parser b
parseSimpleI (str, term) = reserved str >> return term
  --List of simple ITerms without inputs

reservedSimple = [("*", Star), ("Nat", Nat), ("Zero", Zero)]

annTerm :: Parser ITerm
annTerm =
  do reserved "Ann"
     theTerm <- cterm
     theType <- cterm
     return $ Ann theTerm theType
     <|> do
    theTerm <- cterm --Idris annotation syntax
    reservedOp ":"
    theType <- cterm
    return $ Ann theTerm theType

piTerm :: Parser ITerm
piTerm = do
  reserved "Pi"
  input <- cterm
  func <- cterm
  return $ Pi input func

succTerm :: Parser ITerm
succTerm = do
  reserved "Succ"
  num <- cterm
  return $ Succ num

natelimTerm :: Parser ITerm
natelimTerm = do
  reserved "NatElim"
  motive <- cterm
  mZero <- cterm
  inductive <- cterm
  k <- cterm
  return $ NatElim motive mZero inductive k

vecTerm :: Parser ITerm
vecTerm = do
  reserved "Vec"
  theType <- cterm
  numOfElement <- cterm
  return $ Vec theType numOfElement

nilTerm :: Parser ITerm
nilTerm = do
  reserved "Nil"
  theType <- cterm
  return $ Nil theType

consTerm :: Parser ITerm
consTerm = do
  reserved "Cons"
  eleType <- cterm
  numOfElement <- cterm
  newEle <- cterm
  theVec <- cterm
  return $ Cons eleType numOfElement newEle theVec

vecelimTerm :: Parser ITerm
vecelimTerm = do
  reserved "VecElim"
  eleType <- cterm
  motive <- cterm
  mZero <- cterm
  inductive <- cterm
  numOfElement <- cterm
  xs <- cterm
  return $ VecElim eleType motive mZero inductive numOfElement xs

eqTerm :: Parser ITerm
eqTerm = do
  reserved "Eq"
  theType <- cterm
  x <- cterm
  y <- cterm
  return $ Eq theType x y

reflTerm :: Parser ITerm
reflTerm = do
  reserved "Refl"
  theType <- cterm
  x <- cterm
  return $ Refl theType x

eqelimTerm :: Parser ITerm
eqelimTerm = do
  reserved "EqElim"
  theType <- cterm
  motive <- cterm
  refl <- cterm
  x <- cterm
  y <- cterm
  eqxy <- cterm
  return $ EqElim theType motive refl x y eqxy

boundTerm :: Parser ITerm
boundTerm = do
  reserved "Bound"
  index <- bIndex
  return $ Bound (fromInteger index)
  --Parser for de Bruijn indices of bound variables

bIndex :: Parser Integer
bIndex = parens bIndex <|> natural
  --Parser for the global free variable name

gName :: Parser String
gName = parens gName <|> identifier

--Parser for Name data type
localTerm :: Parser Name
localTerm = do
  reserved "Local"
  index <- natural
  return $ Local (fromInteger index)

quoteTerm :: Parser Name
quoteTerm = do
  reserved "Quote"
  index <- natural
  return $ Quote (fromInteger index)

globalTerm :: Parser Name
globalTerm = do
  reserved "Global"
  gname <- gName
  return $ Global gname

name :: Parser Name
name = localTerm <|> quoteTerm <|> globalTerm

freeTerm :: Parser ITerm
freeTerm = do
  reserved "Free"
  fname <- name
  return $ Free fname

appTerm :: Parser ITerm
appTerm = do
  reservedOp ":@:"
  iterm <- term
  cTerm <- cterm
  eof
  return $ iterm :@: cTerm

parseWhole :: Parser a -> Parser a
parseWhole a = do
  whiteSpace
  aterm <- a
  whiteSpace
  eof
  return aterm

term :: Parser ITerm
term =
  Text.Parsec.try appTerm <|> parens term <|>
  foldr1 (<|>) (map parseSimpleI reservedSimple) --ITerms without inputs
   <|>
  annTerm --ITerms with CTerms as input(s).
   <|>
  piTerm <|>
  succTerm <|>
  natelimTerm <|>
  vecTerm <|>
  nilTerm <|>
  consTerm <|>
  vecelimTerm <|>
  eqTerm <|>
  reflTerm <|>
  eqelimTerm <|>
  boundTerm --Bound var
   <|>
  freeTerm --Free var

infTerm :: Parser CTerm
infTerm = do
  reservedOp "Inf"
  iterm <- term
  return $ Inf iterm

lamTerm :: Parser CTerm
lamTerm = do
  reservedOp "Lam"
  cTerm <- cterm
  return $ Lam cTerm

cterm :: Parser CTerm
cterm = parens cterm <|> infTerm <|> lamTerm

parseString :: Parser a -> String -> a
parseString p str =
  case parse p "" str of
    Left e  -> error $ show e
    Right r -> r
