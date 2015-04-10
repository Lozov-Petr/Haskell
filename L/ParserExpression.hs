module ParserExpression where

import Types
import Constants
import ParserBase

---------------------------
comment :: Parser ()
---------------------------
comment = word cComment >> commentTail where
    
    ---------------------------
    commentTail :: Parser ()
    ---------------------------
    commentTail = anySym >>= \c -> if c /= '\n' && c /= '\r' then commentTail else return () 


---------------------------
void :: Parser ()
---------------------------
void = (oneOf voidChars >> void) |!| (comment >> void) |!| return () 

---------------------------
voidCompl :: Parser a -> Parser a
---------------------------
voidCompl p = p >>= (void >>) . return


---------------------------
symV :: Char -> Parser Char
---------------------------
symV = voidCompl . sym


---------------------------
wordV :: String -> Parser ()
---------------------------
wordV = voidCompl . word


---------------------------
numberV :: Parser Integer
---------------------------
numberV = voidCompl number


---------------------------
variableV :: Parser V
---------------------------
variableV = voidCompl variable


---------------------------
num :: Parser E
---------------------------
num = numberV >>= return . Num


---------------------------
var :: Parser E
---------------------------
var = variableV >>= return . Var


---------------------------
eof :: Parser E
---------------------------
eof = voidCompl $ word cEOF >> return EOF


---------------------------
createArray :: Parser E
---------------------------
createArray = voidCompl $ wordV cCreateArray >> exprInBrackets >>= \e -> return (CreateA e)


---------------------------
unMaybe :: a -> Maybe a -> a
---------------------------
unMaybe _   (Just l) = l
unMaybe def _        = def


---------------------------
struct :: Parser E
---------------------------
struct = symV '{' >> opt (oneElem >>= \h -> many0 (symV ',' >> oneElem) >>= return . (h:)) >>= \m -> symV '}' >> return (Struct $ unMaybe [] m) where
    ---------------------------        
    oneElem :: Parser (V, E)
    ---------------------------
    oneElem = variableV >>= \v -> wordV cEqual >> expr >>= return . (,) v


---------------------------
array :: Parser E
---------------------------
array = symV '[' >> opt (expr >>= \e -> many0 (symV ',' >> expr) >>= return . (e:)) >>= \m -> symV ']' >> return (Array $ unMaybe [] m)


---------------------------
getExprInBrackets :: Parser E -> Parser E
---------------------------
getExprInBrackets p = symV '(' >> p >>= \e -> symV ')' >> return e


---------------------------
exprInBrackets :: Parser E
---------------------------
exprInBrackets = getExprInBrackets expr


---------------------------
constExprInBrackets :: Parser E
---------------------------
constExprInBrackets = getExprInBrackets constExpr


---------------------------
primary :: Parser E
---------------------------
primary =  num    |!| eof |!| array      |!| createArray  
       |!| struct |!| var |!| exprInBrackets


---------------------------
constPrimary :: Parser E
---------------------------
constPrimary = num |!| constExprInBrackets


---------------------------
getParserWithSuff :: Parser E -> Parser E
---------------------------
getParserWithSuff p = p >>= addSuffs where

  ---------------------------
  addArraySuff :: (E -> E) -> Parser E
  ---------------------------
  addArraySuff h = symV '[' >> expr >>= \e -> symV ']' >> addSuffs (h e)


  ---------------------------
  addStructSuff :: (V -> E) -> Parser E
  ---------------------------
  addStructSuff h = symV '.' >> variableV >>= \v -> addSuffs (h v)


  ---------------------------
  addSuffs :: E -> Parser E
  ---------------------------
  addSuffs e = addStructSuff (ElemS e) |!| addArraySuff (ElemA e) |!| return e


---------------------------
primaryWithSuff :: Parser E
---------------------------
primaryWithSuff = getParserWithSuff primary


---------------------------
variableWithSuff :: Parser E
---------------------------
variableWithSuff = getParserWithSuff var


---------------------------
getUnary :: Parser E -> Parser E
---------------------------
getUnary p = p |!| unary' ("-", Inv) |!| unary' ("!", Not) where
    unary' (s, f) = voidCompl $ wordV s >> p >>= return . f


---------------------------
unary :: Parser E
---------------------------
unary = getUnary primaryWithSuff


---------------------------
constUnary :: Parser E
---------------------------
constUnary = getUnary constPrimary


---------------------------
operators :: [([(String, E -> E -> E)], Associativity)]
---------------------------
operators = [([("*",Mul),("/",Div),("%",Mod)],LeftAssoc),
             ([("+",Add),("-",Sub)],LeftAssoc),
             ([(">",Grt),("<",Les),("<=",LoE),("==",Eql),("!=",NEq),(">=",GoE)],NotAssoc),
             ([("&&",And)],RightAssoc),
             ([("||",Or )],RightAssoc)]


---------------------------
constExpr :: Parser E
---------------------------
constExpr = getExprParser constUnary operators


---------------------------
expr :: Parser E
---------------------------
expr = getExprParser unary operators


---------------------------
getExprParser :: Parser E -> [([(String, E -> E -> E)], Associativity)] -> Parser E
---------------------------
getExprParser primary list = foldl update (const primary) list $ id where    
  ---------------------------
  update :: ((E -> E) -> Parser E) -> ([(String, E -> E -> E)], Associativity) -> ((E -> E) -> Parser E)
  ---------------------------
  update acc (list, assoc) = result where     
    ---------------------------
    result :: (E -> E) -> Parser E
    ---------------------------
    result hole = p1 >>= \a -> (op >>= \o -> return (newHole a o) >>= p2) |!| (return $ hole a) where
      p1 = acc id 
      p2 = if assoc == NotAssoc then \e -> p1 >>= return . e else result
      newHole a o = if assoc == LeftAssoc then o $ hole a else hole . (o a)
      op = foldl1 (flip (|!|)) $ map (\(s,f) -> wordV s >> return f) list
