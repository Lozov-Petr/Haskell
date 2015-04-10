module ParserStatement where

import Types
import Constants
import ParserBase
import ParserExpression


---------------------------
semicolon :: Parser () 
---------------------------
semicolon = symV ';' >> return ()

---------------------------
skipP :: Parser S
---------------------------
skipP = wordV cSkip >> semicolon >> return Skip


---------------------------
readP :: Parser S
---------------------------
readP = wordV cRead >> symV '(' >> variableWithSuff >>= \e -> symV ')' >> semicolon >> return (Read e)


---------------------------
assignP :: Parser S
---------------------------
assignP = variableWithSuff >>= \l -> wordV cEqual >> expr >>= \r -> semicolon >> return (Assign l r)


---------------------------
writeP :: Parser S
---------------------------
writeP = wordV cWrite >> exprInBrackets >>= \e -> semicolon >> return (Write e)


---------------------------
label :: Parser L
---------------------------
label = opt (variableV) >>= \l -> semicolon >> return (unMaybe cDefLabel l)


---------------------------
labelWithColon :: Parser L
---------------------------
labelWithColon = opt (variableV >>= \l -> symV ':' >> return l) >>= return . unMaybe cDefLabel


---------------------------
breakP :: Parser S
---------------------------
breakP = wordV cBreak >> label >>= return . Break


---------------------------
continueP :: Parser S
---------------------------
continueP = wordV cContinue >> label >>= return . Continue


---------------------------
throwP :: Parser S
---------------------------
throwP = wordV cThrow >> expr >>= \e -> semicolon >> return (Throw e)


---------------------------
ifWithoutL :: L -> Parser S
---------------------------
ifWithoutL l = wordV cIf >> expr >>= \e -> wordV cThen >> statement >>=
	     \tr -> opt (wordV cElse >> statement) >>= return . IfTE l e tr . unMaybe Skip


---------------------------
whileWtihoutL :: L -> Parser S
---------------------------
whileWtihoutL l = wordV cWhile >> expr >>= \e -> wordV cDo >> statement >>= return . While l e


---------------------------
tryWithoutL :: L -> Parser S
---------------------------
tryWithoutL l = wordV cTry >> statement >>= \t -> wordV cCatch 
                           >> constExprInBrackets >>= \e -> statement >>= return . Try l t e


---------------------------
ifOrWhileP :: Parser S
---------------------------
ifOrWhileP = labelWithColon >>= \l -> ifWithoutL l |!| whileWtihoutL l |!| tryWithoutL l


---------------------------
sqP :: Parser S
---------------------------
sqP = symV '{' >> statements where
    
    ---------------------------
    statements :: Parser S
    ---------------------------
    statements = statement >>= \s -> (statements >>= return . Sq s) |!| (symV '}' >> return s)


---------------------------
statement :: Parser S
---------------------------
statement =  skipP  |!| readP  |!| assignP 
         |!| writeP |!| breakP |!| continueP 
         |!| throwP |!| sqP    |!| ifOrWhileP 
