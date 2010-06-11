import Data.Map as Map



data R_Object = R_NULL        -- ^NULL
              | R_Symbol      -- ^a variable name
              | R_Pairlist    -- ^a pairlist object (mainly internal)
              | R_Closure     -- ^a function
              | R_Environment -- ^an environment
              | R_Promise     -- ^an object used to implement lazy evaluation
              | R_Language    -- ^an R language construct
              | R_Special     -- ^an internal function that does not evaluate
                              -- its arguments
              | R_Builtin     -- ^an internal function that evaluates its
                              -- arguments
              | R_Char        -- ^a `scalar' string object (internal only) ***
              | R_Logical     -- ^a vector containing logical values
              | R_Integer     -- ^a vector containing integer values
              | R_Double      -- ^a vector containing real values
              | R_Complex     -- ^a vector containing complex values
              | R_Character   -- ^a vector containing character values
              | R_DotDotDot   -- ^the special variable length argument ***
              | R_Any         -- ^a special type that matches all types: there
                              -- are no objects of this type
              | R_Expression  -- ^an expression object
              | R_List        -- ^a list
              | R_Bytecode    -- ^byte code (internal only) ***
              | R_Externalptr -- ^an external pointer object
              | R_Weakref     -- ^a weak reference object
              | R_Raw         -- ^a vector containing bytes
              | R_S4          -- ^an S4 object which is not a simple object



{-
  BinMinus      "-"    -- ^ Minus, can be unary or binary
  | BinPlus     "+"    -- ^ Plus, can be unary or binary
  | UnNot       "!"    -- ^ Unary not
  | UnBinTilde  "~"    -- ^ Tilde, used for model formulae, can be either unary or binary
  | UnHelp      "?"    -- ^ Help
  | BinSeq      ":"    -- ^ Sequence, binary (in model formulae: interaction)
  | BinMult     "*"    -- ^ Multiplication, binary
  | BinDiv      "/"    -- ^ Division, binary
  | BinExp      "^"    -- ^ Exponentiation, binary
  | BinUndef    "%x%"  -- ^ Special binary operators, x can be replaced by any valid name
  | BinMod      "%%"   -- ^ Modulus, binary
  | BinIntDiv   "%/%"  -- ^ Integer divide, binary
  | BinMatMult  "%*%"  -- ^ Matrix product, binary
  | BinOutPr    "%o%"  -- ^ Outer product, binary
  | BinKronPr   "%x%"  -- ^ Kronecker product, binary
  | BinIntrsct  "%in%" -- ^ Matching operator, binary (in model formulae: nesting)
  | BinLT       "<"    -- ^ Less than, binary
  | BinGT       ">"    -- ^ Greater than, binary
  | BinEQ       "=="   -- ^ Equal to, binary
  | BinGE       ">="   -- ^ Greater than or equal to, binary
  | BinLE       "<="   -- ^ Less than or equal to, binary
  | BinVecAnd   "&"    -- ^ And, binary, vectorized
  | BinAnd      "&&"   -- ^ And, binary, not vectorized
  | BinVecOr    "| "   -- ^ Or, binary, vectorized
  | BinOr       "||"   -- ^ Or, binary, not vectorized
  | BinAsnLeft  "<-"   -- ^ Left assignment, binary
  | BinAsnRight "->"   -- ^ Right assignment, binary
  | BinElmnt    "$"    -- ^ List subset, binary
-}



rResOps = Map.fromList 
rResOps = Map.fromList 
  [ (BinMinus, "-")      -- ^ Minus, can be unary or binary
  , (BinPlus, "+")       -- ^ Plus, can be unary or binary
  , (UnNot, "!")         -- ^ Unary not
  , (UnBinTilde, "~")    -- ^ Tilde, used for model formulae, can be either unary or binary
  , (UnHelp, "?")        -- ^ Help
  , (BinSeq, ":")        -- ^ Sequence, binary (in model formulae: interaction)
  , (BinMult, "*")       -- ^ Multiplication, binary
  , (BinDiv, "/")        -- ^ Division, binary
  , (BinExp, "^")        -- ^ Exponentiation, binary
  , (BinUndef, "%x%")    -- ^ Special binary operators, x can be replaced by any valid name
  , (BinMod, "%%")       -- ^ Modulus, binary
  , (BinIntDiv, "%/%")   -- ^ Integer divide, binary
  , (BinMatMult, "%*%")  -- ^ Matrix product, binary
  , (BinOutPr, "%o%")    -- ^ Outer product, binary
  , (BinKronPr, "%x%")   -- ^ Kronecker product, binary
  , (BinIntrsct, "%in%") -- ^ Matching operator, binary (in model formulae: nesting)
  , (BinLT, "<")         -- ^ Less than, binary
  , (BinGT, ">")         -- ^ Greater than, binary
  , (BinEQ, "==")        -- ^ Equal to, binary
  , (BinGE, ">=")        -- ^ Greater than or equal to, binary
  , (BinLE, "<=")        -- ^ Less than or equal to, binary
  , (BinVecAnd, "&")     -- ^ And, binary, vectorized
  , (BinAnd, "&&")       -- ^ And, binary, not vectorized
  , (BinVecOr, "| ")     -- ^ Or, binary, vectorized
  , (BinOr, "||")        -- ^ Or, binary, not vectorized
  , (BinAsnLeft, "<-")   -- ^ Left assignment, binary
  , (BinAsnRight, "->")  -- ^ Right assignment, binary
  , (BinElmnt, "$")      -- ^ List subset, binary
  ]
  [ (BinMinus, "-")      -- ^ Minus, can be unary or binary
  , (BinPlus, "+")       -- ^ Plus, can be unary or binary
  , (UnNot, "!")         -- ^ Unary not
  , (UnBinTilde, "~")    -- ^ Tilde, used for model formulae, can be either unary or binary
  , (UnHelp, "?")        -- ^ Help
  , (BinSeq, ":")        -- ^ Sequence, binary (in model formulae: interaction)
  , (BinMult, "*")       -- ^ Multiplication, binary
  , (BinDiv, "/")        -- ^ Division, binary
  , (BinExp, "^")        -- ^ Exponentiation, binary
  , (BinUndef, "%x%")    -- ^ Special binary operators, x can be replaced by any valid name
  , (BinMod, "%%")       -- ^ Modulus, binary
  , (BinIntDiv, "%/%")   -- ^ Integer divide, binary
  , (BinMatMult, "%*%")  -- ^ Matrix product, binary
  , (BinOutPr, "%o%")    -- ^ Outer product, binary
  , (BinKronPr, "%x%")   -- ^ Kronecker product, binary
  , (BinIntrsct, "%in%") -- ^ Matching operator, binary (in model formulae: nesting)
  , (BinLT, "<")         -- ^ Less than, binary
  , (BinGT, ">")         -- ^ Greater than, binary
  , (BinEQ, "==")        -- ^ Equal to, binary
  , (BinGE, ">=")        -- ^ Greater than or equal to, binary
  , (BinLE, "<=")        -- ^ Less than or equal to, binary
  , (BinVecAnd, "&")     -- ^ And, binary, vectorized
  , (BinAnd, "&&")       -- ^ And, binary, not vectorized
  , (BinVecOr, "| ")     -- ^ Or, binary, vectorized
  , (BinOr, "||")        -- ^ Or, binary, not vectorized
  , (BinAsnLeft, "<-")   -- ^ Left assignment, binary
  , (BinAsnRight, "->")  -- ^ Right assignment, binary
  , (BinElmnt, "$")      -- ^ List subset, binary
  ]


data R_ResOps = BinMinus | BinPlus | UnNot | UnBinTilde 
              | UnHelp | BinSeq | BinMult | BinDiv | BinExp 
              | BinUndef | BinMod | BinIntDiv | BinMatMult 
              | BinOutPr | BinKronPr | BinIntrsct | BinLT 
              | BinGT | BinEQ | BinGE | BinLE | BinVecAnd 
              | BinAnd | BinVecOr | BinOr | BinAsnLeft 
              | BinAsnRight | BinElmnt 
  deriving (Read, Ord, Show, Eq, Enum)
