module ParserExpression where

import Types
import Constants
import ParserBase


---------------------------
void :: Parser ()
---------------------------
void = (oneOf Nothing voidChars >> return()) |!| comment where
  
  ---------------------------
  comment :: Parser ()
  ---------------------------
  comment = word Nothing cComment >> commentTail
    
  ---------------------------
  commentTail :: Parser ()
  ---------------------------
  commentTail = anySym Nothing >>= \c -> if c /= '\n' && c /= '\r' then commentTail else return () 


---------------------------
voids :: Parser Bool
---------------------------
voids = (void >> voids0 >> return True) |!| return False where

  ---------------------------
  voids0 :: Parser ()
  ---------------------------
  voids0 = void >> voids0 |!| return () 


---------------------------
voidsCompl :: Parser a -> Parser a
---------------------------
voidsCompl p = p >>= \a -> voids >> return a


---------------------------
symV :: Char -> Parser Bool
---------------------------
symV c = sym Nothing c >> voids


---------------------------
wordV :: String -> Parser Bool
---------------------------
wordV s = word Nothing s >> voids


---------------------------
numberV :: Parser Integer
---------------------------
numberV = voidsCompl $ number Nothing


---------------------------
variableV :: Parser V
---------------------------
variableV = voidsCompl $ variable Nothing


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
eof = wordV cEOF >> return EOF


---------------------------
createArray :: Parser E
---------------------------
createArray = voidsCompl $ wordV cCreateArray >> exprInBrackets >>= \e -> return (CreateA e)


---------------------------
unMaybe :: a -> Maybe a -> a
---------------------------
unMaybe _   (Just l) = l
unMaybe def _        = def


---------------------------
struct :: Parser E
---------------------------
struct = symV '{' >> opt (oneElem >>= \h -> many0 (symV ',' >> oneElem) >>= return . (h:)) 
     >>= \m -> symV '}' >> return (Struct $ unMaybe [] m) where
    ---------------------------        
    oneElem :: Parser (V, E)
    ---------------------------
    oneElem = variableV >>= \v -> wordV cAssign >> expr >>= return . (,) v


---------------------------
array :: Parser E
---------------------------
array = symV '[' >> opt (expr >>= \e -> many0 (symV ',' >> expr) >>= return . (e:)) >>= \m -> symV ']' >> return (Array $ unMaybe [] m)


---------------------------
inBrackets :: Parser a -> Parser a
---------------------------
inBrackets p = symV '(' >> p >>= \a -> symV ')' >> return a


---------------------------
exprInBrackets :: Parser E
---------------------------
exprInBrackets = inBrackets expr


---------------------------
constExprInBrackets :: Parser E
---------------------------
constExprInBrackets = inBrackets constExpr


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
    unary' (s, f) = wordV s >> p >>= return . f


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
operators = [([("^",Pow)],NotAssoc),
             ([("*",Mul),("/",Div),("%",Mod)],LeftAssoc),
             ([("+",Add),("-",Sub)],LeftAssoc),
             ([(">",Grt),("<",Les),("<=",LoE),("==",Eql),("!=",NEq),(">=",GoE)],NotAssoc),
             ([("&&",And)],RightAssoc),
             ([("||",Or )],RightAssoc)]


---------------------------
constExprWithoutIf :: Parser E
---------------------------
constExprWithoutIf = getExprParser constUnary operators


---------------------------
exprWithoutIf :: Parser E
---------------------------
exprWithoutIf = getExprParser unary operators


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


---------------------------
addIfExpr :: Parser E -> Parser E
---------------------------
addIfExpr p = par where
  par = p >>= \c -> (pt >>= \t1 -> pf >>= \f1 -> let i = If c t1 f1 in (pt >>= \t2 -> pf >>= return . If i t2) |!| return i)
                |!| return c
  pt = symV '?' >> par
  pf = symV ':' >> par


---------------------------
expr :: Parser E
---------------------------
expr = addIfExpr exprWithoutIf


---------------------------
constExpr :: Parser E
---------------------------
constExpr = addIfExpr constExprWithoutIf