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
afterWord :: Parser a -> Parser a -> Bool -> Parser a
---------------------------
afterWord a b w = if w then a else b


---------------------------
exprAfterWord :: Bool -> Parser E
---------------------------
exprAfterWord = afterWord expr exprInBrackets


---------------------------
constExprAfterWord :: Bool -> Parser E
---------------------------
constExprAfterWord = afterWord constExpr constExprInBrackets


---------------------------
statementAfterWord :: Bool -> Parser S
---------------------------
statementAfterWord = afterWord statement (sqP |!| emptyP)


---------------------------
emptyP :: Parser S
---------------------------
emptyP = semicolon >> return Skip


---------------------------
skipP :: Parser S
---------------------------
skipP = wordV cSkip >> semicolon >> return Skip |!| emptyP


---------------------------
abortP :: Parser S
---------------------------
abortP = wordV cAbort >> semicolon >> return Abort


---------------------------
readP :: Parser S
---------------------------
readP = wordV cRead >> symV '(' >> tailRead id where
  
  ---------------------------
  tailRead :: (S -> S) -> Parser S
  ---------------------------
  tailRead h = variableWithSuff >>= \v -> (symV ')' >> semicolon >> return (h $ Read v))
                                      |!| (symV ',' >> tailRead (h . Sq (Read v)))


---------------------------
writeP :: Parser S
---------------------------
writeP = wordV cWrite >> symV '(' >> tailWrite id where

  ---------------------------
  tailWrite :: (S -> S) -> Parser S
  ---------------------------
  tailWrite h = expr >>= \e -> (symV ')' >> semicolon >> return (h $ Write e))
                           |!| (symV ',' >> tailWrite (h . Sq (Write e)))

---------------------------
assignP :: Parser S
---------------------------
assignP = variableWithSuff >>= \l -> assign l >>= \f -> expr >>= \r -> semicolon >> return (Assign l (f r)) where

  ---------------------------
  assign :: E -> Parser (E -> E)
  ---------------------------
  assign l = foldl1 (|!|) $ map (\(a,b) -> wordV a >> return b) assigns where

    assigns = [(cAssign,    id)   , (cAddAssign, Add l), 
               (cSubAssign, Sub l), (cMulAssign, Mul l),
               (cDivAssign, Div l), (cModAssign, Mod l),
               (cPowAssign, Pow l)] 


---------------------------
label :: Parser L
---------------------------
label = opt variableV >>= \l -> semicolon >> return (unMaybe cDefLabel l)


---------------------------
breakP :: Parser S
---------------------------
breakP = wordV cBreak >>= afterWord (label >>= return . Break) 
                                    (semicolon >> return (Break cDefLabel))


---------------------------
continueP :: Parser S
---------------------------
continueP = wordV cContinue >>= afterWord (label >>= return . Continue)
                                          (semicolon >> return (Continue cDefLabel))


---------------------------
throwP :: Parser S
---------------------------
throwP = wordV cThrow >>= exprAfterWord >>= \e -> semicolon >> return (Throw e)


---------------------------
ifP :: Parser S
---------------------------
ifP = wordV cIf >>= exprAfterWord >>= \e -> wordV cThen >>= statementAfterWord >>= \tr -> ifTail (IfTE e tr) where
            
            ---------------------------
            ifTail :: (S -> S) -> Parser S
            ---------------------------
            ifTail hole =  (wordV cElse >>= statementAfterWord >>= return . hole) 
                       |!| (wordV cElif >>= exprAfterWord >>= \e -> wordV cThen >>= 
                                            statementAfterWord >>= \s -> ifTail (hole . IfTE e s))
                       |!| return (hole Skip)


---------------------------
tryP :: Parser S
---------------------------
tryP = wordV cTry >>= afterWord statementWithoutTry sqP >>= catchesP where

    ---------------------------
    catchesP :: S -> Parser S
    ---------------------------
    catchesP s = wordV cCatch >>= constExprAfterWord >>= \e -> symV ':' >> statement >>= 
    	    \c -> let s1 = Try s e c in catchesP s1 |!| return s1


---------------------------
switchP :: Parser S
---------------------------
switchP = wordV cSwitch >>= exprAfterWord >>= \e -> many0 caseP >>= 
       \c -> defP >>= return . Switch e (concat c) where

    defP = opt (wordV cDefault >>= statementAfterWord) >>= return . unMaybe Skip

    ---------------------------
    caseP :: Parser [(E, S)]
    ---------------------------
    caseP = wordV cCase >>= constExprAfterWord >>= \e -> many0 (symV ',' >> constExpr) 
                        >>= \es -> symV ':' >> statement >>= \s -> return . map (flip (,) s) $ e:es 



---------------------------
labelWithColon :: Parser L
---------------------------
labelWithColon = opt (variableV >>= \l -> symV ':' >> return l) >>= return . unMaybe cDefLabel


---------------------------
whileWtihoutL :: L -> Parser S
---------------------------
whileWtihoutL l = wordV cWhile >>= exprAfterWord >>= \e -> 
                  wordV cDo >>= statementAfterWord >>= return . While l e


---------------------------
repeatWtihoutL :: L -> Parser S
---------------------------
repeatWtihoutL l = wordV cRepeat >>= statementAfterWord >>= \s -> 
                   wordV cUntil >>= exprAfterWord >>= \e -> semicolon >> return (Repeat l s e)


---------------------------
forWithoutL :: L -> Parser S
---------------------------
forWithoutL l =  word Nothing cFor >> void >> voids >> variableV >>= 
           \c -> wordV cAssign >> expr >>=
           \b -> wordV cTo >>= exprAfterWord >>=
           \e -> opt (wordV cStep >>= exprAfterWord) >>=
           \s -> wordV cDo >>= statementAfterWord >>= 
                 return . For l c b e (unMaybe (Num 1) s)


---------------------------
ciclesP :: Parser S
---------------------------
ciclesP = labelWithColon >>= \l -> whileWtihoutL l |!| repeatWtihoutL l |!| forWithoutL l


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
