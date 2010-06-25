{
module Language.R.Scanner where 
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Map as Map

}

%wrapper "posn"

$nonzero_digit = 1-9
$digit = [$nonzero_digit 1]
$alpha = [a-zA-Z]
$ident_first_char = [$alpha _ \.]
$ident_other_char = [$alpha $digit _ \.]
$eol_char = [\n \r]
$n_eol_char = ~$eol_char
$white_no_eol = $white # $eol_char

$short_str_char = [^ \n \r ' \" \\] -- "
$long_str_char = [. \n] # [' \"] -- "
$short_byte_str_char = \0-\127 # [\n \r ' \" \\] -- "
$long_byte_str_char = \0-\127 # [' \"] -- "
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \" -- "

@reserved_op 
  =  "-" | "+"   | "!"   | "~"   | "?"   | ":"    | "*"  | "/"  | "^"  
  | "%%" | "%/%" | "%*%" | "%o%" | "%x%" | "%in%" | "<"  | ">"  | "%x%"
  | "==" | ">="  | "<="  | "&"   |  "&&" |  "|"   | "||" | "<-" | "->" 
  | "$"  

@keywords 
  = break|next|continue

@flow_ctrl
  = if|for|while|repeat|return|function
  
@math_builtins
  = abs|sign|sqrt|floor|ceiling|exp|expm1|log2|log10|log1p
  |cos|sin|tan|acos|asin|atan|cosh|sinh|tanh|acosh|asinh
  |atanh|gamma|lgamma|digamma|trigamma|cumsum|cumprod
  |cummax|cummin|Im|Re|Arg|Conj|Mod
  
@cast_builtins
  = as.call|as.character|as.complex|as.double|as.environment|as.integer
  |as.logical|as.raw
  
@refect_builtins
  = nargs|missing|interactive|is.xxx.Primitive|.Internal|globalenv
  |baseenv|emptyenv|pos.to.env|unclass|invisible|browser
  
@seq_builtins
  = seq_along|seq_len

@system_builtins
  = proc.time|gc.time|tracemem|retracemem|untracemem

@primitives
  = c|list|call|expression|substitute|UseMethod|standardGeneric
  |.C|.Fortran|.Call|.External|.Call.graphics|.External.graphics
  |.subset|.subset2|.primTrace|.primUntrace|round|signif
  |rep|seq.int|lazyLoadDBfetch

@replaceable
  = length|class|oldCLass|attr|attributes|names|dim|dimnames
  |environment|levels|storage.mode

@replacement
  = length "<-"|class "<-"|oldCLass"<-" |attr"<-" |attributes"<-"
  |names"<-"|dim"<-"|dimnames"<-"|environment"<-"|levels"<-"
  |storage.mode"<-"

tokens :-

  \" $short_str_char* \"               { tok (\p s -> StringToken p (tail $ init s) ) }
  "'" $not_single_quote* "'"           { tok (\p s -> StringToken p (tail $ init s) ) }
  $eol_char+                           { tok (\p s -> LinebreakToken p) }
  $white_no_eol+                       ;
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
  $ident_first_char $ident_other_char* { tok (\p s -> classifyIdentifier p s) }

{

data IdentifierType 
  = BuiltinIdent
  | KeywordIdent
  | PrimitiveIdent
  | UserIdent
  deriving (Read, Show, Eq)



classifyIdentifier :: AlexPosn -> String -> Token
classifyIdentifier p st =
  let isBuiltin = st `elem` builtinsList
      isKeyword = st `elem` keywordList
      isOperat  = st `elem` operatorList
      idType = case (isBuiltin, isKeyword, isOperat) of
                 (_, True, _) -> KeywordIdent
                 (True, _, _) -> BuiltinIdent
                 (_, _, _   ) -> UserIdent
  in IdentifierToken p idType st



tok f p s = f p s

-- The token type:
data Token =
  DebugLinebreakToken                    |
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
  StringToken            AlexPosn String | 
  IntegerToken           AlexPosn Int    | 
  ImaginaryToken         AlexPosn Double | 
  NumericToken           AlexPosn Double | 
  ScientificNumericToken AlexPosn String | 
  Sym                    AlexPosn Char   | 
  Var                    AlexPosn String | 
  Int                    AlexPosn Int    |
  IdentifierToken        AlexPosn IdentifierType String
  deriving (Eq,Show)



sshow :: Token -> Doc
sshow (LinebreakToken _) = black $ onwhite $ string "\n------------LINE_BREAK_HERE-------------\n"
sshow (TrueToken p)  = bold $ red $ string $ (spacePad p) ++ (show (TrueToken p))
sshow (FalseToken p) = bold $ red $ string $ (spacePad p) ++ (show (FalseToken p))
sshow (NA_Token p)   = dullcyan $ string $ (spacePad p) ++ (show (NA_Token p))
sshow (NaN_Token p)  = dullcyan $ string $ (spacePad p) ++ (show (NaN_Token p))
sshow (NULL_Token p) = dullcyan $ string $ (spacePad p) ++ (show (NULL_Token p))
sshow (Inf_Token p)  = dullcyan $ string $ (spacePad p) ++ (show (Inf_Token p))

sshow (CommaToken p)             = blue $ string $ (spacePad p) ++  (show (CommaToken p))
sshow (ParenLeftToken p)         = blue $ string $ (spacePad p) ++ (show (ParenLeftToken p))
sshow (ParenRightToken p)        = blue $ string $ (spacePad p) ++ (show (ParenRightToken p))
sshow (BracketLeftToken p)       = blue $ string $ (spacePad p) ++ (show (BracketLeftToken p))
sshow (BracketRightToken p)      = blue $ string $ (spacePad p) ++ (show (BracketRightToken p))
sshow (BraceLeftToken p)         = blue $ string $ (spacePad p) ++ (show (BraceLeftToken p))
sshow (BraceRightToken p)        = blue $ string $ (spacePad p) ++ (show (BraceRightToken p))
sshow (PlusToken p)              = magenta $ string $ (spacePad p) ++ (show (PlusToken p))
sshow (MinusToken p)             = magenta $ string $ (spacePad p) ++ (show (MinusToken p))
sshow (MulitplyToken p)          = magenta $ string $ (spacePad p) ++ (show (MulitplyToken p))
sshow (DivideToken p)            = magenta $ string $ (spacePad p) ++ (show (DivideToken p))
sshow (PowerExponentialToken p)  = magenta $ string $ (spacePad p) ++ (show (PowerExponentialToken p))
sshow (RightArrowToken p)        = magenta $ string $ (spacePad p) ++ (show (RightArrowToken p))
sshow (DoubleRightArrowToken p)  = magenta $ string $ (spacePad p) ++ (show (DoubleRightArrowToken p))
sshow (LeftArrowToken p)         = magenta $ string $ (spacePad p) ++ (show (LeftArrowToken p))
sshow (DoubleLeftArrowToken p)   = magenta $ string $ (spacePad p) ++ (show (DoubleLeftArrowToken p))
sshow (DotToken p)               = blue $ string $ (spacePad p) ++ (show (DotToken p))
sshow (EllipsisToken p)          = magenta $ string $ (spacePad p) ++ (show (EllipsisToken p))
sshow (TildeToken p)             = magenta $ string $ (spacePad p) ++ (show (TildeToken p))
sshow (DivToken p)               = magenta $ string $ (spacePad p) ++ (show (DivToken p))
sshow (LessThanToken p)          = magenta $ string $ (spacePad p) ++ (show (LessThanToken p))
sshow (LessThanEqualsToken p)    = magenta $ string $ (spacePad p) ++ (show (LessThanEqualsToken p))
sshow (GreaterThanToken p)       = magenta $ string $ (spacePad p) ++ (show (GreaterThanToken p))
sshow (GreaterThanEqualsToken p) = magenta $ string $ (spacePad p) ++ (show (GreaterThanEqualsToken p))
sshow (EqualityToken p)          = magenta $ string $ (spacePad p) ++ (show (EqualityToken p))
sshow (NotEqualsToken p)         = magenta $ string $ (spacePad p) ++ (show (NotEqualsToken p))
sshow (BinaryOrToken p)          = magenta $ string $ (spacePad p) ++ (show (BinaryOrToken p))
sshow (AndToken p)               = magenta $ string $ (spacePad p) ++ (show (AndToken p))
sshow (BinaryAndToken p)         = magenta $ string $ (spacePad p) ++ (show (BinaryAndToken p))
sshow (OrToken p)                = magenta $ string $ (spacePad p) ++ (show (OrToken p))
sshow (ColonToken p)             = blue $ string $ (spacePad p) ++ (show (ColonToken p))
sshow (AssignToken p)            = magenta $ string $ (spacePad p) ++ (show (AssignToken p))
sshow (AtToken p)                = magenta $ string $ (spacePad p) ++ (show (AtToken p))
sshow (BackQuoteToken p)         = blue $ string $ (spacePad p) ++ (show (BackQuoteToken p))
sshow (DollarSignToken p)        = blue $ string $ (spacePad p) ++ (show (DollarSignToken p))
sshow (SemiToken p)              = blue $ string $ (spacePad p) ++ (show (SemiToken p))

sshow (StringToken p s)            = bold $ green $ string $ (spacePad p) ++ ( show (StringToken p s) )
sshow (IntegerToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (IntegerToken p s) )
sshow (ImaginaryToken p s)         = bold $ cyan $ string $ (spacePad p) ++ ( show (ImaginaryToken p s) )
sshow (NumericToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (NumericToken p s) )
sshow (ScientificNumericToken p s) = bold $ cyan $ string $ (spacePad p) ++ ( show (ScientificNumericToken p s) )
sshow (Sym p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Sym p s) )
sshow (Var p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Var p s) )
sshow (Int p s)                    = dullwhite $ string $ (spacePad p) ++ ( show (Int p s) )

sshow (IdentifierToken p t s) = bold $ yellow  $ string $ (spacePad p) ++ ( show (IdentifierToken p t s) )

spacePad :: AlexPosn -> String
spacePad (AlexPn offset col row) = concat $ replicate (row-1) " "

tokpos :: AlexPosn -> (Int, Int)
tokpos (AlexPn o c r) = (c, r)


splitByLines :: [Token] -> [([Token], Int)]
splitByLines toks = 
  let row         = fst $ tokpos $ token_posn $ head toks
      (car, cdr)  = span (\z -> (fst $ tokpos $ token_posn z) == row) toks
  in (car, row):(splitByLines cdr)


-- prettyPrintPair :: Doc -> String -> IO ()
prettyPrintPair (lns, orig) = do
  putStrLn ("\n" ++ orig ++ "\n")
  putStrLn (show lns)
  return ()


-- prettyPrintTokens :: [Token] -> String -> IO ()
prettyPrintTokens toks inp = do
  let splits   = splitByLines toks
      lins     = lines inp
      outs     = map (\(tk, rw) -> (vcat (map sshow tk), lins !! (rw - 1))) splits
  -- mapM_ prettyPrintPair outs
  return outs
      

  
token_posn (StringToken p _) = p
token_posn (IdentifierToken p _ _) = p
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



-- operatorList :: [String]
operatorList = [ "..." , ":" , "::" , "?" , "??" , "["
               , "[[" , "$" , "@" , "+" , "!" , "<-" 
               , "->" , "<<-" , "=" , "[<-" , "[[<-"
               , "<-" , "-" , "*" , "/" , "^" , "%%"
               , "%*%" , "<" , "<=" , "==" , "!=" , ">="
               , ">" , "|" , "||" , "&" , "&&" , "%/%" ]


keywordList :: [String]
keywordList = 
  [ "if"
  , "for"
  , "while"
  , "repeat"
  , "return"
  , "function"
  , "quote"
  , "switch"
  , "break"
  , "next"
  , "length"
  , "length<-"
  , "class"
  , "class<-"
  , "oldClass"
  , "oldCLass<-"
  , "attr"
  , "attr<-"
  , "attributes"
  , "attributes<-"
  , "names"
  , "names<-"
  , "dim"
  , "dim<-"
  , "dimnames"
  , "dimnames<-"
  , "levels<-"
  , "environment<-"
  , "storage.mode<-" ]


builtinsList :: [String]
builtinsList = 
  [ "abs"
  , "sign"
  , "sqrt"
  , "floor"
  , "ceiling"
  , "exp"
  , "expm1"
  , "log2"
  , "log10"
  , "log1p"
  , "cos"
  , "sin"
  , "tan"
  , "acos"
  , "asin"
  , "atan"
  , "cosh"
  , "sinh"
  , "tanh"
  , "acosh"
  , "asinh"
  , "atanh"
  , "gamma"
  , "lgamma"
  , "digamma"
  , "trigamma"
  , "cumsum"
  , "cumprod"
  , "cummax"
  , "cummin"
  , "Im"
  , "Re"
  , "Arg"
  , "Conj"
  , "Mod" ]

runScanner st = alexScanTokens st

{-
main = do
  s <- getContents
  print (alexScanTokens s)
-}
}
