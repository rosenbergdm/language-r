{
-- module Language.R.Scanner where 
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
  = "::" | "@" | "$" | "^" | "-U" | "+U" | ":" | "%%" | "%/%" 
  | "%*%" | "%o%" | "%x%" | "%in%" | "*" | "/" | "-B" | "+B" 
  | "<" | ">" | "==" | "!=" | ">=" | "<=" | "!" | "&" | "&&" 
  | "|" | "||" | "~U" | "~B" | "->" | "->>" | "=" | "<-" 
  | "<<-"        

@keyword 
  = break|next|continue

@flow_ctrl
  = if|for|while|else|repeat|return|function
  
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

@lit_logical 
  = TRUE|FALSE

@nops 
  = NA|NaN|Inf|NULL


tokens :-

  
  $eol_char+                           { tok (\p s -> LinebreakToken p) }
  $white_no_eol+                       ; -- No-op whitespace 
  "#".*                                ; -- Comment 

  \" $short_str_char* \"               { tok (\p s -> StringToken p (tail $ init s) ) }
  "'" $not_single_quote* "'"           { tok (\p s -> StringToken p (tail $ init s) ) }

  $digit+ "L"                          { tok (\p s -> IntegerToken p (read (init s) :: Int ) ) }
  $digit+ "." $digit+ "E" "-"? $digit+ { tok (\p s -> ScientificNumericToken p s ) }
  $digit+ "." $digit+ "i"              { tok (\p s -> ImaginaryToken p (read (init s) :: Double) ) }
  $digit+ "." $digit+                  { tok (\p s -> NumericToken p (read s :: Double ) ) }
  $digit+                              { tok (\p s -> NumericToken p (fromIntegral (read s :: Int) :: Double ) ) }

  @lit_logical                         { tok (\p s -> mkLogicalLitTok p s) }

  @nops                                { tok (\p s -> mkSpecialToken p s) }

  @reserved_op                         { tok (\p s -> getOpToken p s ) }
  @keyword                             { tok (\p s -> getKeywdToken p s) }
  @flow_ctrl                           { tok (\p s -> getFCToken p s) }


  ","                                  { tok (\p s -> CommaToken p ) }
  "("                                  { tok (\p s -> ParenLeftToken p ) }
  ")"                                  { tok (\p s -> ParenRightToken p ) }
  "["                                  { tok (\p s -> BracketLeftToken p ) }
  "]"                                  { tok (\p s -> BracketRightToken p ) }
  "{"                                  { tok (\p s -> BraceLeftToken p ) }
  "}"                                  { tok (\p s -> BraceRightToken p ) }
  ";"                                  { tok (\p s -> SemiToken p ) }
  "."                                  { tok (\p s -> DotToken p ) }
  "`"                                  { tok (\p s -> BackQuoteToken p ) }

  $ident_first_char $ident_other_char* { tok (\p s -> classifyIdentifier p s) }

{

mkLogicalLitTok :: AlexPosn -> String -> Token
mkLogicalLitTok p "TRUE"  = LiteralToken p (LogicalLiteral [True ]) 
mkLogicalLitTok p "FALSE" = LiteralToken p (LogicalLiteral [False]) 

mkSpecialToken :: AlexPosn -> String -> Token
mkSpecialToken p "NA"   = NA_Token   p
mkSpecialToken p "NaN"  = NaN_Token  p
mkSpecialToken p "NULL" = NULL_Token p
mkSpecialToken p "Inf"  = Inf_Token  p

getKeywdToken :: AlexPosn -> String -> Token
getKeywdToken p "break"    = KeywordToken p BreakKeyword
getKeywdToken p "next"     = KeywordToken p NextKeyword
getKeywdToken p "continue" = KeywordToken p ContinueKeyword


getFCToken :: AlexPosn -> String -> Token
getFCToken p "if" = FlowControlToken p IfFCToken
getFCToken p "for" = FlowControlToken p ForFCToken
getFCToken p "while"  = FlowControlToken p WhileFCToken
getFCToken p "else"  = FlowControlToken p ElseFCToken 
getFCToken p "repeat"  = FlowControlToken p RepeatFCToken
getFCToken p "return" = FlowControlToken p ReturnFCToken
getFCToken p "function" = FlowControlToken p  FunctionFCToken

data FlowCtrlTokenType
  = IfFCToken
  | ForFCToken
  | WhileFCToken
  | ElseFCToken
  | RepeatFCToken
  | ReturnFCToken
  | FunctionFCToken
  deriving (Show, Eq)


data IdentifierType 
  = BuiltinIdent
  | KeywordIdent
  | PrimitiveIdent
  | UserIdent
  deriving (Read, Show, Eq)

data OpAssocDir = AssocLeft | AssocRight 
  deriving (Show, Eq)

data BinOpPrec = BinOpPrec
  { bPrecOrder :: Int
  , assocDir   :: OpAssocDir
  } deriving (Show, Eq)

data UnOpPrec = UnOpPrec
  { uPrecOrder :: Int
  } deriving (Show, Eq)

opTokenMap :: Map.Map String (Either BinOpPrec UnOpPrec)
opTokenMap = Map.fromList  
  [ ("::",   Left (BinOpPrec 0 AssocLeft ))
  , ("@",    Left (BinOpPrec 1 AssocLeft ))
  , ("$",    Left (BinOpPrec 1 AssocLeft ))
  
  , ("^",    Left (BinOpPrec 2 AssocRight ))
  
  , ("-U",    Right (UnOpPrec 3 ))
  , ("+U",    Right (UnOpPrec 3 ))
  
  , (":",    Left (BinOpPrec 4 AssocRight ))

  , ("%%",   Left (BinOpPrec 5 AssocLeft ))
  , ("%/%",  Left (BinOpPrec 5 AssocLeft ))
  , ("%*%",  Left (BinOpPrec 5 AssocLeft ))
  , ("%o%",  Left (BinOpPrec 5 AssocLeft ))
  , ("%x%",  Left (BinOpPrec 5 AssocLeft ))
  , ("%in%", Left (BinOpPrec 5 AssocLeft ))
  
  , ("*",    Left (BinOpPrec 6 AssocLeft ))
  , ("/",    Left (BinOpPrec 6 AssocLeft ))
  
  , ("-B",    Left (BinOpPrec 7 AssocLeft ))
  , ("+B",    Left (BinOpPrec 7 AssocLeft ))

  , ("<",    Left (BinOpPrec 8 AssocLeft ))
  , (">",    Left (BinOpPrec 8 AssocLeft ))
  , ("==",   Left (BinOpPrec 8 AssocLeft ))
  , ("!=",   Left (BinOpPrec 8 AssocLeft ))
  , (">=",   Left (BinOpPrec 8 AssocLeft ))
  , ("<=",   Left (BinOpPrec 8 AssocLeft ))
  
  , ("!",    Right (UnOpPrec 9 ))
  
  , ("&",    Left (BinOpPrec 10 AssocLeft ))
  , ("&&",   Left (BinOpPrec 10 AssocLeft ))
  
  , ("|",    Left (BinOpPrec 11 AssocLeft ))
  , ("||",   Left (BinOpPrec 11 AssocLeft ))
  
  , ("~U",    Left (BinOpPrec 12 AssocLeft ))
  , ("~B",    Right (UnOpPrec 12 ))

  , ("->",   Left (BinOpPrec 13 AssocLeft ))
  , ("->>",  Left (BinOpPrec 13 AssocLeft ))
  
  , ("=",    Left (BinOpPrec 14 AssocRight ))

  , ("<-",   Left (BinOpPrec 15 AssocRight ))
  , ("<<-",  Left (BinOpPrec 15 AssocRight ))
  ]


getOpToken :: AlexPosn -> String -> Token
getOpToken p s = 
  let ub_prec = Map.lookup s opTokenMap
      tok     = case ub_prec of 
                   Just (Left  (BinOpPrec pr dr)) -> BinOpToken p (BinOpPrec pr dr) s
                   Just (Right (UnOpPrec  pr   )) -> UnOpToken  p (UnOpPrec  pr   ) s
                   Nothing                        -> ErrorToken p s
  in tok

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

data LiteralTokenType 
  = LogicalLiteral [Bool] 
  | IntegerLiteral [Int]
  | NumericLiteral [Double]
  | ComplexLiteral [(Double, Double)]
  | CharacterLiteral [String]
  deriving (Show, Eq)


data KeywordTokenType
  = BreakKeyword
  | ContinueKeyword
  | NextKeyword
  deriving (Show, Eq)



tok f p s = f p s

-- The token type:
data Token =
  ErrorToken             AlexPosn String            |
  LiteralToken           AlexPosn LiteralTokenType  |
  BinOpToken             AlexPosn BinOpPrec String  |
  UnOpToken              AlexPosn UnOpPrec String   |
  KeywordToken           AlexPosn KeywordTokenType  |
  FlowControlToken       AlexPosn FlowCtrlTokenType |

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
  DotToken               AlexPosn        | 
  EllipsisToken          AlexPosn        | 
  BackQuoteToken         AlexPosn        | 
  SemiToken              AlexPosn        | 
  StringToken            AlexPosn String | 
  IntegerToken           AlexPosn Int    | 
  ImaginaryToken         AlexPosn Double | 
  NumericToken           AlexPosn Double | 
  ScientificNumericToken AlexPosn String | 
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
sshow (DotToken p)               = blue $ string $ (spacePad p) ++ (show (DotToken p))
sshow (EllipsisToken p)          = magenta $ string $ (spacePad p) ++ (show (EllipsisToken p))
sshow (BackQuoteToken p)         = blue $ string $ (spacePad p) ++ (show (BackQuoteToken p))
sshow (SemiToken p)              = blue $ string $ (spacePad p) ++ (show (SemiToken p))

sshow (StringToken p s)            = bold $ green $ string $ (spacePad p) ++ ( show (StringToken p s) )
sshow (IntegerToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (IntegerToken p s) )
sshow (ImaginaryToken p s)         = bold $ cyan $ string $ (spacePad p) ++ ( show (ImaginaryToken p s) )
sshow (NumericToken p s)           = bold $ cyan $ string $ (spacePad p) ++ ( show (NumericToken p s) )
sshow (ScientificNumericToken p s) = bold $ cyan $ string $ (spacePad p) ++ ( show (ScientificNumericToken p s) )

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
token_posn (DotToken p ) = p
token_posn (EllipsisToken p ) = p
token_posn (BackQuoteToken p ) = p
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


main = do
  s <- getContents
  print (alexScanTokens s)

}
