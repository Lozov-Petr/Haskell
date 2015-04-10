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
skipP = opt (wordV0 cSkip) >> semicolon >> return Skip


---------------------------
abortP :: Parser S
---------------------------
abortP = wordV0 cAbort >> semicolon >> return Abort


---------------------------
readP :: Parser S
---------------------------
readP = wordV0 cRead >> symV '(' >> variableWithSuff >>= \e -> symV ')' >> semicolon >> return (Read e)


---------------------------
assignP :: Parser S
---------------------------
assignP = variableWithSuff >>= \l -> wordV0 cEqual >> expr >>= \r -> semicolon >> return (Assign l r)


---------------------------
writeP :: Parser S
---------------------------
writeP = wordV0 cWrite >> exprInBrackets >>= \e -> semicolon >> return (Write e)


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
breakP = wordV1 cBreak >> label >>= return . Break


---------------------------
continueP :: Parser S
---------------------------
continueP = wordV1 cContinue >> label >>= return . Continue


---------------------------
throwP :: Parser S
---------------------------
throwP = wordV0 cThrow >> exprInBrackets >>= \e -> semicolon >> return (Throw e)


---------------------------
ifWithoutL :: L -> Parser S
---------------------------
ifWithoutL l = wordV1 cIf >> expr >>= \e -> wordV1 cThen >> statement >>=
	     \tr -> opt (wordV1 cElse >> statement) >>= return . IfTE l e tr . unMaybe Skip


---------------------------
whileWtihoutL :: L -> Parser S
---------------------------
whileWtihoutL l = wordV1 cWhile >> expr >>= \e -> wordV1 cDo >> statement >>= return . While l e


---------------------------
tryWithoutL :: L -> Parser S
---------------------------
tryWithoutL l = wordV1 cTry >> statement >>= \t -> wordV0 cCatch 
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
         |!| throwP |!| sqP    |!| abortP 
         |!| ifOrWhileP 
