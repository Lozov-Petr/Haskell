module Parser where

import Types 
import ParserBase
import ParserExpression
import ParserStatement

---------------------------
program :: Parser P
---------------------------
program = voids0 >> statement >>= return . Program

---------------------------
parser :: String -> Either P Error
---------------------------
parser = apply program
