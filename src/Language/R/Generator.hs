-----------------------------------------------------------------------------
-- |
-- Module      : Language.R.Generator
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 05/28/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

module Language.R.Generator where

import Language.R.Internal

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

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
--rWhiteSpace = whiteSpace     rlang 
rBraces     = braces         rlang 
rOperator   = operator       rlang 
rSemi       = semi           rlang 
rString     = stringLiteral  rlang


-- tokParser  :: Parser RToken


{-
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement ::  Parser Stmt
statement =   parens statement
          <|> seqOfStmt

seqOfStmt = do
  list <- (sepBy1 statement' semi)
  res <- if length list == 1
           then head list
         else Seq list
  return res

statement' ::  Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

-}

{- { {{
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt
 
assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

}}} -}
{- {{{
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement
Now because any statement might be actually a sequence of statements separated by semicolon, we use sepBy1 to parse at least one statement. The result is a list of statements. We also allow grouping statements by the parenthesis, which is useful, for instance, in the while loop.

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt
 
sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list
Now a single statement is quite simple, it's either an if conditional, a while loop, an assignment or simply a skip statement. We use <|> to express choice. So a <|> b will first try parser a and if it fails (but without actually consuming any input) then parser b will be used. Note: this means that the order is important.

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
}}} -}



{- {{{
emptyDef    
 = LanguageDef 
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = ""
   , nestedComments = True
   , identStart     = letter <|> char '_'
   , identLetter    = alphaNum <|> oneOf "_'"
   , opStart        = opLetter emptyDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = True
   }

javaStyle :: LanguageDef st
This is a minimal token definition for Java style languages. It defines the style of comments, valid identifiers and case sensitivity. It does not define any reserved words or operators.
javaStyle   
    = emptyDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = True
    , identStart     = letter
    , identLetter    = alphaNum <|> oneOf "_'"
    , reservedNames  = []
    , reservedOpNames= []	
    , caseSensitive  = False	   
    }

haskellStyle :: LanguageDef st
This is a minimal token definition for Haskell style languages. It defines the style of comments, valid identifiers and case sensitivity. It does not define any reserved words or operators.
haskellStyle
    = emptyDef
    { commentStart   = "{-"
    , commentEnd     = "-}"
    , commentLine    = "--"
    , nestedComments = True
    , identStart     = letter
    , identLetter    = alphaNum <|> oneOf "_'"
    , opStart        = opLetter haskell
    , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"              
    , reservedOpNames= []
    , reservedNames  = []
    , caseSensitive  = True                                   
    }         

mondrianDef :: LanguageDef st
The language definition for the language Mondrian.
mondrian    
    = javaStyle
    { reservedNames  = [ "case", "class", "default"
                       , "extends"
                       , "import", "in", "let"
                       , "new", "of", "package"
                       ]	
    , caseSensitive  = True
    }

haskell98Def :: LanguageDef st
The language definition for the language Haskell98.
haskell98Def     
    = haskellStyle
    { reservedOpNames= ["::","..","=","\\","|"
                       ,"<-","->","@","~","=>"
                       ]
    , reservedNames  = ["let","in","case","of"
                       ,"if","then","else"
                       ,"data","type",
                       ,"class","default","deriving"
                       ,"do","import",
                       ,"infix","infixl","infixr"
                       ,"instance","module"
                       ,"newtype","where"
                       ,"primitive"
                        -- "as","qualified","hiding"
                       ]
    }               

haskellDef :: LanguageDef st
The language definition for the Haskell language.
haskellDef   
  = haskell98Def
    { identLetter    = identLetter haskell98Def <|> char '#'
    , reservedNames  = reservedNames haskell98Def ++ 
                       ["foreign","import","export","primitive"
                       ,"_ccall_","_casm_"
                       ,"forall"
                       ]
    }

}}} -}

{-
mondrian :: TokenParser st
A lexer for the mondrian language.
mondrian  = makeTokenParser mondrianDef

haskell :: TokenParser st
A lexer for the haskell language.
mondrian  = makeTokenParser haskellDef


-}
