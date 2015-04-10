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
exprInBrackets :: Parser E
---------------------------
exprInBrackets = symV '(' >> expr >>= \e -> symV ')' >> return e


---------------------------
primary :: Parser E
---------------------------
primary =  num    |!| eof |!| array      |!| createArray  
       |!| struct |!| var |!| exprInBrackets


---------------------------
primaryWithSuff :: Parser E
---------------------------
primaryWithSuff = primary >>= addSuffs where

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
unary :: Parser E
---------------------------
unary = primaryWithSuff |!| unary' ("-", Inv) |!| unary' ("!", Not) where
    unary' (s, f) = voidCompl $ wordV s >> primaryWithSuff >>= return . f


---------------------------
expr :: Parser E
---------------------------
expr = getExprParser unary [([("*",Mul),("/",Div),("%",Mod)],LeftAssoc),
                            ([("+",Add),("-",Sub)],LeftAssoc),
                            ([(">",Grt),("<",Les),("<=",LoE),("==",Eql),("!=",NEq),(">=",GoE)],NotAssoc),
                            ([("&&",And)],RightAssoc),
                            ([("||",Or )],RightAssoc)]


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


