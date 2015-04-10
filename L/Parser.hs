module Parser where

import Types 
import ParserBase (Parser, apply)
import ParserExpression (void)
import ParserStatement (statement)

---------------------------
program :: Parser P
---------------------------
program = void >> statement >>= return . Program

---------------------------
parser :: String -> P
---------------------------
parser = apply program
