module Parser where

import Types 
import ParserBase
import ParserExpression
import ParserStatement

---------------------------
program :: Parser P
---------------------------
program = voids >> sqP >>= return . Program

---------------------------
parser :: String -> Either P Error
---------------------------
parser = apply program
