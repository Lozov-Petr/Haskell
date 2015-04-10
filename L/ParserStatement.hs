module ParserStatement where

import Types
import Constants
import ParserBase
import ParserExpression

---------------------------
elemP :: Parser Elem
---------------------------
elemP = variableV >>= addSuffsS . EV where

	---------------------------
	addArraySuffS :: (E -> Elem) -> Parser Elem
	---------------------------
	addArraySuffS h = symV '[' >> expr >>= \e -> symV ']' >> addSuffsS (h e)


	---------------------------
	addStructSuffS :: (V -> Elem) -> Parser Elem
	---------------------------
	addStructSuffS h = symV '.' >> variableV >>= \v -> addSuffsS (h v)


	---------------------------
	addSuffsS :: Elem -> Parser Elem
	---------------------------
	addSuffsS e = addStructSuffS (ES e) |!| addArraySuffS (EA e) |!| return e


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
readP = wordV cRead >> symV '(' >> elemP >>= \e -> symV ')' >> semicolon >> return (Read e)


---------------------------
assignP :: Parser S
---------------------------
assignP = elemP >>= \l -> wordV cEqual >> expr >>= \r -> semicolon >> return (Assign l r)


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
tryWithoutL l = wordV cTry >> statement >>= \t -> wordV cCatch >> exprInBrackets >>= \e -> statement >>= return . Try l t e


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
statement = skipP |!| readP |!| assignP |!| writeP |!| breakP |!| continueP |!| ifOrWhileP |!| sqP
