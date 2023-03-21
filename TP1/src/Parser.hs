module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           Text.Parsec.Combinator
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = (ignoreparensint assignintexp) `chainl1` commaintexp

commaintexp :: Parser (Exp Int -> Exp Int -> Exp Int)
commaintexp = 
    do 
      reservedOp lis ","
      return (ESeq)
    <|> do 
      sumintexp

assignintexp :: Parser (Exp Int)
assignintexp =
    try (do {
      valor <- identifier lis;
      reservedOp lis "=";
      y <- (ignoreparensint assignintexp) `chainl1` sumintexp;
      return (EAssgn valor y);
    })
    <|> do
      simpleintexp

sumintexp :: Parser (Exp Int -> Exp Int -> Exp Int)
sumintexp = 
    do
      reservedOp lis "+"
      return (Plus)
    <|> do
      reservedOp lis "-"
      return (Minus)
    <|> do
      reservedOp lis "*"
      return (Times)
    <|> do
      reservedOp lis "/"
      return (Div)

simpleintexp :: Parser (Exp Int)
simpleintexp =
    do
      valor <- identifier lis
      return (Var valor)
    <|> do 
      valor <- natural lis
      return (Const (fromIntegral valor))
    <|> try (do {
              reservedOp lis "-";
              ignoreparensint (do {
                                 valor <- natural lis;
                                  return (UMinus (Const (fromInteger valor))) 
                               });
      })
   

ignoreparensint :: Parser (Exp Int) -> Parser (Exp Int)
ignoreparensint parser = 
    do 
      parens lis intexp
    <|> do
      parser
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = (ignoreParensBool notboolexp) `chainl1` orboolexp

orboolexp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orboolexp = 
    do
      reservedOp lis "||"
      return (Or)
    <|> do
      reservedOp lis "&&"
      return (And)

notboolexp :: Parser (Exp Bool)
notboolexp = 
    do
      reservedOp lis "!"
      x <- ignoreParensBool mediumboolexp
      return (Not x)
    <|> do
      x <- ignoreParensBool mediumboolexp
      return (x)

mediumboolexp :: Parser (Exp Bool)
mediumboolexp =
    do 
      comparisonsboolexp
    <|> do
      simpleboolexp


comparisonsboolexp :: Parser (Exp Bool)
comparisonsboolexp = 
    (do
      operando1 <- intexp
      (do
         reservedOp lis ">"
         operando2 <- intexp
         return (Gt operando1 operando2)
       <|> do
         reservedOp lis "<"
         operando2 <- intexp
         return (Lt operando1 operando2)
       <|> do
         reservedOp lis "!="
         operando2 <- intexp
         return (NEq operando1 operando2)
       <|> do
         (reservedOp lis "==")
         operando2 <- intexp
         return (Eq operando1 operando2)))


simpleboolexp :: Parser (Exp Bool)
simpleboolexp = 
    do
      reservedOp lis "true"
      return (BTrue)
    <|> do
      reservedOp lis "false"
      return (BFalse)

ignoreParensBool :: Parser (Exp Bool) -> Parser (Exp Bool)
ignoreParensBool parser = 
    do 
      parens lis boolexp
    <|> do
      parser
      
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = (commaux) `chainl1` (do { reservedOp lis ";" ; return (Seq) })

commaux :: Parser Comm
commaux = 
    do 
      reservedOp lis "skip"
      return Skip
    <|> do
      reservedOp lis "if"
      operando1 <- boolexp
      operando2 <- braces lis comm
      (do 
         reservedOp lis "else"
         operando3 <- braces lis comm
         return (IfThenElse operando1 operando2 operando3)
       <|> do
         return (IfThen operando1 operando2))
    <|> do
      reservedOp lis "repeat"
      operando1 <- braces lis comm
      reservedOp lis "until"
      operando2 <- boolexp
      return (Repeat operando1 operando2)
    <|> do
      nombre <- identifier lis
      reservedOp lis "="
      valor <- intexp
      return (Let nombre valor)




------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

test1 = parseComm "error" "skip;skip;skip"
test2 = parseComm "error" "if true { skip } else { skip; repeat { skip } until true}"
test3 = parseComm "error" "x = y = 3, 90 - 5 * 90, -3"
test4 = parseComm "error" "if true && false || false {skip} else {skip; if 3 < 5 {skip} else {skip}}"
test5 = parseComm "error" "if false && true {skip}"
test6 = parseComm "error" "if (false || (true || false)) {skip};if (((false || true) || false)) {skip}"
test7 = parseComm "error" "x = (y = 3), (90 - 5) * 90, -(3), (3 - 2) - 1, 3 - (2 - 1)"
test70 = parseComm "error" "x = (90 - 5) * 90"
test8 = parseComm "error" "x = 3 + (y = z = q = 4) "
test80 = parseComm "error" "x = y = z = 0"
test9 = parseComm "error" "x = x + y + 5 * (z = 4 * 12 + 1) / abcdefghi"
test90 = parseComm "error" "x = x + y + 5 * z = 4 * 12 + 1 / abcdefghi"
test901 = parseComm "error" "m = 5 * z = 4 * 12 + 1"
test91 = parseComm "error" "m = (x = 3) + (y = z = q = 4)"
test10 = parseComm "error" "x = x + y +zrfsfs"
test11 = parseComm "error" "x = x + y +23"
test12 = parseComm "error" "if b > a {c = 1} else{c = 0} ; a = a / 0"
testN = parseTest (comm >> Text.Parsec.Combinator.parserTrace "label") 

right x = case x of 
          Right y -> y 
          _ -> undefined

superTest = do { test1; test2; test3; test4; test5 ; test6 ; test7 ; test70 ; test8; test80 ;test9;test90;test901;test91; test10 ; test11; test12 }

