module ParserBase where

newtype Parser a = P (String -> Maybe (a,String))

instance Monad Parser where
	return a = P (Just . (,) a)
	(P p) >>= f = P (\s -> p s >>= \(a,s) -> let (P p) = f a in p s)


---------------------------
empty :: Parser a
--------------------------
empty = P $ const Nothing


--------------------------
anySym :: Parser Char
--------------------------
anySym = P anySym' where
  anySym' (x:xs) = Just (x,xs)
  anySym'  _     = Nothing

---------------------------
sym :: Char -> Parser Char
---------------------------
sym c = P sym' where
	sym' (x:xs) | c == x = Just (x,xs)
	sym'  _              = Nothing

---------------------------
infixl 1 |!|
(|!|) :: Parser a -> Parser a -> Parser a
---------------------------
(P p1) |!| (P p2) = P (\s -> let res = p1 s in if isNothing res then p2 s else res) where
	isNothing Nothing = True
	isNothing _       = False


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
apply :: Parser a -> String -> a
---------------------------
apply (P p) s = result $ p s where
  result (Just (a,b)) = a
  result  _       = error "Nothing"


---------------------------
oneOf :: [Char] -> Parser Char
---------------------------
oneOf = foldr ((|!|) . sym) empty



---------------------------
word :: String -> Parser ()
---------------------------
word []     = return ()
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
number =  many1 digit >>= return . read


---------------------------
variable :: Parser String
---------------------------
variable = letter >>= \x  -> many0 (letter |!| digit) >>= return . (x:)