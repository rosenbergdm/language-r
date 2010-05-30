

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language



-----------------------------------------------------------------------------
-- Tokens
-----------------------------------------------------------------------------

-- |Parsing is performed using a two-pass lexer - parser sequence to generate
-- an abstract syntax tree.  This allows disambiguation and proper handling
-- of peculiarities of the R language.

data Tok = StrTok    -- ^String Token
         | NumTok    -- ^Number Token
         | AtomTok   -- ^Atom Token
         | SymTok    -- ^Symbol Token
         | ComTok    -- ^Comment Token
  deriving (Eq, Ord, Show, Enum)


-- |Tokens are defined by their 'type', the literal string used to represent
-- them in the source code, and the location where they occured.

data RToken  =  RToken
  { tokType    :: Tok         -- ^Token type
  , tokLiteral :: String      -- ^The literal token string
--  , tokPos     :: SourcePos   -- ^The location (row, column) of the occurance
  } deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------
-- Basic data types
-----------------------------------------------------------------------------

-- |Internally, R normally uses doubles for storing all numeric values
-- (even when they are displayed as integers).  

data RNumType = RInt  -- ^Declared as integer
              | RNum  -- ^Declared as numeric
  deriving (Eq, Ord, Show)

data RNumeric = RNumeric
  { rnVals   :: [Double]
  , decAs    :: RNumType
  , rnNames  :: [String] }
  deriving (Eq, Ord, Show)

-- |Character vectors are represented with standard Haskell Strings.

data RCharacter = RCharacter 
  { rcVals     :: [String] 
  , rcNames    :: [String] }
  deriving (Eq, Ord, Show)


-- |Boolean vectors are represented with the standard Haskell Boolean.

data RBoolean = RBoolean 
  { rbVals   :: [Bool] 
  , rbNames  :: [String] }
  deriving (Eq, Ord, Show)

-- |Vectors form the most basic / atomic of R's data types
-- Vectors are required to be homogenous.
data RVector = Rnumeric [Double] RNumType (Maybe [String])
             | Rcharachter [String] (Maybe [String])
             | Rboolean [Bool] (Maybe [String])
  deriving (Eq, Ord, Show)


data RList = RList 
  { rlRows :: [RVector] }




rStyle :: LanguageDef st
rStyle = emptyDef
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = "#"
   , nestedComments = False
   , identStart     = letter <|> char '.'
   , identLetter    = alphaNum <|> oneOf "._"
   , opStart        = opLetter emptyDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= ["<-", "<<-", "->", "[[", "]]"]
   , reservedNames  = [ "if", "else", "repeat", "while",
                        "function", "for", "in", "next", 
                        "break", "TRUE", "FALSE", "NULL",
                        "Inf", "NaN", "NA", "NA_integer_",
                        "NA_real_","NA_complex_", 
                        "NA_character_" 
                      ]
   , caseSensitive  = True
   }

rlang :: TokenParser st
rlang =  makeTokenParser rStyle

rIdentifier = identifier     rlang 
rNumber     = naturalOrFloat rlang 
rReserved   = reserved       rlang 
rReservedOp = reservedOp     rlang 
rParens     = parens         rlang 
rWhiteSpace = whiteSpace     rlang 
rBraces     = braces         rlang 
rOperator   = operator       rlang 
rSemi       = semi           rlang 
rString     = stringLiteral  rlang

{-


-- rToks ::  TokParser [RToken] pos
rToks :: Parser [RToken]
rToks = many1 rToken

rToken = rString <|> rNumber <|> rReserved <|> rIdentifier


getStrTok = do
  lit <- rString
  return $ RToken StrTok lit

getNumTok = do
  num <- rNumber
  return $ RToken NumTok (num)

-- getSymTok = do
--   opr <- rReservedOp <|> rOperator
--   return $ RToken SymTok (show opr)

getAtomTok = do
  atm <- rIdentifier <|> rReserved
  return $ RToken AtomTok (show atm)
-}
