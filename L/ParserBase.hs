module ParserBase where

type ErrorMessage = Maybe String

type Error = (ErrorMessage,Int,Int)
type Str   = (String,Int,Int)

newtype Parser a = P (Str -> Either (a, Str) Error)

instance Monad Parser where
  return a = P (Left . (,) a)
  (P p) >>= f = P $ next f . p where  
    next f (Left (a,s)) = let (P p) = f a in p s
    next _ (Right err)  = Right err 


--------------------------
anySym :: Parser Char
--------------------------
anySym = P anySym' where
  anySym' (x:xs,i,j) | x == '\n' = Left (x,(xs,i+1,1))
                     | True      = Left (x,(xs,i,j+1))
  anySym' (_,   i,j)             = Right (Nothing,i,j)             


---------------------------
sym :: Char -> Parser Char
---------------------------
sym c = P sym' where
  sym' (x:xs,i,j) | c == x = if c == '\n' then Left (x,(xs,i+1,1)) 
                                          else Left (x,(xs,i,j+1))
  sym' (_   ,i,j)          = Right (Just $ "Expexted " ++ show c,i,j)              

---------------------------
infixl 1 |!|
(|!|) :: Parser a -> Parser a -> Parser a
---------------------------
(P p1) |!| (P p2) = P (\s -> result (p1 s) $ p2 s) where
  result a@(Left _)  _          = a
  result _           b@(Left _) = b
  result l@(Right (e1,i1,j1)) r@(Right (e2,i2,j2)) | i1 > i2 || i1 == i2 && j1 > j2 = l
                                                   | True                           = r

---------------------------
many0 :: Parser a -> Parser [a]
---------------------------
many0 p = (p >>= \a -> many0 p >>= return .(a:)) |!| return []


---------------------------
many1 :: Parser a -> Parser [a]
---------------------------
many1 p = p >>= \a -> many0 p >>= return .(a:)


---------------------------
opt :: Parser a -> Parser (Maybe a)
---------------------------
opt p = p >>= return . Just  |!| return Nothing


---------------------------
apply :: Parser a -> String -> Either a Error
---------------------------
apply (P p) s = result $ p (s,1,1) where
  ---------------------------
  result :: Either (a,Str) Error -> Either a Error
  result (Left (a,([],_,_))) = Left a
  result (Left (_,(_ ,i,j))) = Right (Just "Err",i,j)
  result (Right err)         = Right err



---------------------------
oneOf :: [Char] -> Parser Char
---------------------------
oneOf = foldr1 (|!|) . map sym


---------------------------
word :: String -> Parser ()
---------------------------
word   []     = return ()
word (x:xs) = sym x >> word xs


---------------------------
digit :: Parser Char
---------------------------
digit = oneOf ['0'..'9']


---------------------------
letter :: Parser Char
---------------------------
letter = oneOf ('_' : (['a'..'z'] ++ ['A'..'Z']))

---------------------------
number :: Parser Integer
---------------------------
number = many1 digit >>= return . read


---------------------------
variable :: Parser String
---------------------------
variable = letter >>= \x  -> many0 (letter |!| digit) >>= return . (x:)
