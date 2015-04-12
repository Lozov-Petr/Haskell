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
ifP :: Parser S
---------------------------
ifP = wordV1 cIf >> expr >>= \e -> wordV1 cThen >> statement >>= \tr -> ifTail (IfTE e tr) where
            
            ---------------------------
            ifTail :: (S -> S) -> Parser S
            ---------------------------
            ifTail hole =  (wordV1 cElse >> statement >>= return . hole) 
                       |!| (wordV1 cElif >> expr >>= \e -> wordV1 cThen >> 
                                            statement >>= \s -> ifTail (hole . IfTE e s))
                       |!| return (hole Skip)


---------------------------
tryP :: Parser S
---------------------------
tryP = wordV1 cTry >> statementWithoutTry >>= catchesP where

    ---------------------------
    catchesP :: S -> Parser S
    ---------------------------
    catchesP s = wordV0 cCatch >> constExprInBrackets >>= \e -> statement >>= 
    	    \c -> let s1 = Try s e c in catchesP s1 |!| return s1


---------------------------
switchP :: Parser S
---------------------------
switchP = wordV0 cSwitch >> exprInBrackets >>= \e -> many0 caseP >>= 
	      \c -> wordV1 cDefault >> statement >>= return . Switch e c where

    ---------------------------
    caseP :: Parser (E, S)
    ---------------------------
    caseP = wordV0 cCase >> constExprInBrackets >>= \e -> statement >>= return . (,) e


---------------------------
whileWtihoutL :: L -> Parser S
---------------------------
whileWtihoutL l = wordV1 cWhile >> expr >>= \e -> wordV1 cDo >> statement >>= return . While l e


---------------------------
ciclesP :: Parser S
---------------------------
ciclesP = labelWithColon >>= whileWtihoutL


---------------------------
sqP :: Parser S
---------------------------
sqP = symV '{' >> ((symV '}' >> return Skip) |!| statements) where
    
    ---------------------------
    statements :: Parser S
    ---------------------------
    statements = statement >>= \s -> (statements >>= return . Sq s) |!| (symV '}' >> return s)


---------------------------
statementWithoutTry :: Parser S
---------------------------
statementWithoutTry =  skipP   |!| readP   |!| ifP
                   |!| writeP  |!| breakP  |!| continueP 
                   |!| throwP  |!| sqP     |!| abortP
                   |!| switchP |!| assignP |!| ciclesP


---------------------------
statement :: Parser S
---------------------------
statement =  tryP |!| statementWithoutTry 
