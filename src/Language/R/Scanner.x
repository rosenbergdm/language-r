{
module Main where
}

%wrapper "posn"

$nonzero_digit = 1-9
$digit = [$nonzero_digit 1]
$alpha = [a-zA-Z]
$ident_first_char = [$alpha _ \.]
$ident_other_char = [$alpha $digit _ \.]
$eol_char = [\n \r]
$n_eol_char = ~$eol_char

$short_str_char = [^ \n \r ' \" \\] -- "
$long_str_char = [. \n] # [' \"] -- "
$short_byte_str_char = \0-\127 # [\n \r ' \" \\] -- "
$long_byte_str_char = \0-\127 # [' \"] -- "
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \" -- "

tokens :-

  \" $short_str_char* \"               { tok (\p s -> StringToken p (tail $ init s) ) }
  "'" $not_single_quote* "'"           { tok (\p s -> StringToken p (tail $ init s) ) }
  $eol_char+                           { tok (\p s -> LinebreakToken p) }
  $white+                              ;
  "#".*                                ; 
  $digit+ "L"                          { tok (\p s -> IntegerToken p (read (init s) :: Int ) ) }
  $digit+ "." $digit+ "E" "-"? $digit+ { tok (\p s -> ScientificNumericToken p s ) }
  $digit+ "." $digit+ "i"              { tok (\p s -> ImaginaryToken p (read (init s) :: Double) ) }
  $digit+ "." $digit+                  { tok (\p s -> NumericToken p (read s :: Double ) ) }
  $digit+                              { tok (\p s -> NumericToken p (fromIntegral (read s :: Int) :: Double ) ) }
  "TRUE"                               { tok (\p s -> TrueToken p ) }
  "FALSE"                              { tok (\p s -> FalseToken p ) }
  "NA"                                 { tok (\p s -> NA_Token p ) }
  "NaN"                                { tok (\p s -> NaN_Token p ) }
  "Inf"                                { tok (\p s -> Inf_Token p ) }
  "NULL"                               { tok (\p s -> NULL_Token p ) }
  ","                                  { tok (\p s -> CommaToken p ) }
  "("                                  { tok (\p s -> ParenLeftToken p ) }
  ")"                                  { tok (\p s -> ParenRightToken p ) }
  "["                                  { tok (\p s -> BracketLeftToken p ) }
  "]"                                  { tok (\p s -> BracketRightToken p ) }
  "{"                                  { tok (\p s -> BraceLeftToken p ) }
  "}"                                  { tok (\p s -> BraceRightToken p ) }
  ";"                                  { tok (\p s -> SemiToken p ) }
  "+"                                  { tok (\p s -> PlusToken p ) }
  "-"                                  { tok (\p s -> MinusToken p ) }
  "*"                                  { tok (\p s -> MulitplyToken p ) }
  "/"                                  { tok (\p s -> DivideToken p ) }
  "^"                                  { tok (\p s -> PowerExponentialToken p ) }
  "->"                                 { tok (\p s -> RightArrowToken p ) }
  "->>"                                { tok (\p s -> DoubleRightArrowToken p ) }
  "<-"                                 { tok (\p s -> LeftArrowToken p ) }
  "<<-"                                { tok (\p s -> DoubleLeftArrowToken p ) }
  "."                                  { tok (\p s -> DotToken p ) }
  "..."                                { tok (\p s -> EllipsisToken p ) }
  "~"                                  { tok (\p s -> TildeToken p ) }
  "/"                                  { tok (\p s -> DivToken p ) }
  "<"                                  { tok (\p s -> LessThanToken p ) }
  "<="                                 { tok (\p s -> LessThanEqualsToken p ) }
  ">"                                  { tok (\p s -> GreaterThanToken p ) }
  ">="                                 { tok (\p s -> GreaterThanEqualsToken p ) }
  "=="                                 { tok (\p s -> EqualityToken p ) }
  "!="                                 { tok (\p s -> NotEqualsToken p ) }
  "|"                                  { tok (\p s -> BinaryOrToken p ) }
  "&&"                                 { tok (\p s -> AndToken p ) }
  "&"                                  { tok (\p s -> BinaryAndToken p ) }
  "||"                                 { tok (\p s -> OrToken p ) }
  ":"                                  { tok (\p s -> ColonToken p ) }
  "="                                  { tok (\p s -> AssignToken p ) }
  "@"                                  { tok (\p s -> AtToken p ) }
  "`"                                  { tok (\p s -> BackQuoteToken p ) }
  "$"                                  { tok (\p s -> DollarSignToken p ) }
  $ident_first_char $ident_other_char+ { tok (\p s -> IdentifierToken p s ) }

  [\=\+\-\*\/\(\)]      { tok (\p s -> Sym p (head s)) }
  $alpha [$alpha $digit \_ \']*    { tok (\p s -> Var p s) }

{
-- Each action has type :: String -> Token

tok f p s = f p s

-- The token type:
data Token =
  LinebreakToken         AlexPosn        | 
  Let                    AlexPosn        | 
  In                     AlexPosn        | 
  TrueToken              AlexPosn        | 
  FalseToken             AlexPosn        | 
  NA_Token               AlexPosn        | 
  NaN_Token              AlexPosn        | 
  NULL_Token             AlexPosn        | 
  Inf_Token              AlexPosn        | 
  CommaToken             AlexPosn        | 
  ParenLeftToken         AlexPosn        | 
  ParenRightToken        AlexPosn        | 
  BracketLeftToken       AlexPosn        | 
  BracketRightToken      AlexPosn        | 
  BraceLeftToken         AlexPosn        | 
  BraceRightToken        AlexPosn        | 
  PlusToken              AlexPosn        | 
  MinusToken             AlexPosn        | 
  MulitplyToken          AlexPosn        | 
  DivideToken            AlexPosn        | 
  PowerExponentialToken  AlexPosn        | 
  RightArrowToken        AlexPosn        | 
  DoubleRightArrowToken  AlexPosn        | 
  LeftArrowToken         AlexPosn        | 
  DoubleLeftArrowToken   AlexPosn        | 
  DotToken               AlexPosn        | 
  EllipsisToken          AlexPosn        | 
  TildeToken             AlexPosn        | 
  DivToken               AlexPosn        | 
  LessThanToken          AlexPosn        | 
  LessThanEqualsToken    AlexPosn        | 
  GreaterThanToken       AlexPosn        | 
  GreaterThanEqualsToken AlexPosn        | 
  EqualityToken          AlexPosn        | 
  NotEqualsToken         AlexPosn        | 
  BinaryOrToken          AlexPosn        | 
  AndToken               AlexPosn        | 
  BinaryAndToken         AlexPosn        | 
  OrToken                AlexPosn        | 
  ColonToken             AlexPosn        | 
  AssignToken            AlexPosn        | 
  AtToken                AlexPosn        | 
  BackQuoteToken         AlexPosn        | 
  DollarSignToken        AlexPosn        | 
  SemiToken              AlexPosn        | 
  IdentifierToken        AlexPosn String | 
  StringToken            AlexPosn String | 
  IntegerToken           AlexPosn Int    | 
  ImaginaryToken         AlexPosn Double | 
  NumericToken           AlexPosn Double | 
  ScientificNumericToken AlexPosn String | 
  Sym                    AlexPosn Char   | 
  Var                    AlexPosn String | 
  Int                    AlexPosn Int
  deriving (Eq,Show)


token_posn (StringToken p _) = p
token_posn (IdentifierToken p _) = p
token_posn (IntegerToken p _) = p
token_posn (ImaginaryToken p _) = p
token_posn (NumericToken p _) = p
token_posn (ScientificNumericToken p _) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
token_posn (LinebreakToken p ) = p
token_posn (Let p ) = p
token_posn (In p ) = p
token_posn (TrueToken p ) = p
token_posn (FalseToken p ) = p
token_posn (NA_Token p ) = p
token_posn (NaN_Token p ) = p
token_posn (NULL_Token p ) = p
token_posn (Inf_Token p ) = p
token_posn (CommaToken p ) = p
token_posn (ParenLeftToken p ) = p
token_posn (ParenRightToken p ) = p
token_posn (BracketLeftToken p ) = p
token_posn (BracketRightToken p ) = p
token_posn (BraceLeftToken p ) = p
token_posn (BraceRightToken p ) = p
token_posn (PlusToken p ) = p
token_posn (MinusToken p ) = p
token_posn (MulitplyToken p ) = p
token_posn (DivideToken p ) = p
token_posn (PowerExponentialToken p ) = p
token_posn (RightArrowToken p ) = p
token_posn (DoubleRightArrowToken p ) = p
token_posn (LeftArrowToken p ) = p
token_posn (DoubleLeftArrowToken p ) = p
token_posn (DotToken p ) = p
token_posn (EllipsisToken p ) = p
token_posn (TildeToken p ) = p
token_posn (DivToken p ) = p
token_posn (LessThanToken p ) = p
token_posn (LessThanEqualsToken p ) = p
token_posn (GreaterThanToken p ) = p
token_posn (GreaterThanEqualsToken p ) = p
token_posn (EqualityToken p ) = p
token_posn (NotEqualsToken p ) = p
token_posn (BinaryOrToken p ) = p
token_posn (AndToken p ) = p
token_posn (BinaryAndToken p ) = p
token_posn (OrToken p ) = p
token_posn (ColonToken p ) = p
token_posn (AssignToken p ) = p
token_posn (AtToken p ) = p
token_posn (BackQuoteToken p ) = p
token_posn (DollarSignToken p ) = p
token_posn (SemiToken p ) = p


main = do
  s <- getContents
  print (alexScanTokens s)
}