module ParserBase where

import Messages
import Types
import Data.Maybe(isJust)

--------------------------
anySym :: ErrorMessage -> Parser Char
--------------------------
anySym err = P $ anySym' err where
  anySym'  _           (x:xs,i,j) | x == '\n' = Left (x,(xs,i+1,1))
                                  | True      = Left (x,(xs,i,j+1))
  anySym' err@(Just _) (_,   i,j)             = Right (err,i,j) 
  anySym' _            (_,   i,j)             = Right (mEOF,i,j)           


---------------------------
sym :: ErrorMessage -> Char -> Parser Char
---------------------------
sym err c = P $ sym' err where
  sym' _            (x:xs,i,j) | c == x = if c == '\n' then Left (x,(xs,i+1,1)) 
                                                       else Left (x,(xs,i,j+1))
  sym' err@(Just _) (_   ,i,j)          = Right (err,i,j) 
  sym' _            (_   ,i,j)          = Right (mExpected $ show c,i,j)             

---------------------------
infixl 1 |!|
(|!|) :: Parser a -> Parser a -> Parser a
---------------------------
(P p1) |!| (P p2) = P (\s -> result (p1 s) $ p2 s) where
  result l@(Left _)  _          = l
  result _           r@(Left _) = r
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
  result (Left (_,(_ ,i,j))) = Right (Just "Err",i,j) -- !!! --
  result (Right err)         = Right err



---------------------------
oneOf :: ErrorMessage -> [Char] -> Parser Char
---------------------------
oneOf err@(Just _) l = foldr1 (|!|) $ map (sym err) l
oneOf _            l = foldr1 (|!|) $ map (sym . mExpected $ chars l) l where
  chars [x]    = [x]
  chars (x:xs) = x:',':' ':chars xs


---------------------------
word :: ErrorMessage ->  String -> Parser ()
---------------------------
word err s = word' s where
  err' = if isJust err then err else mExpected $ show s
  ---------------------------
  word' :: String -> Parser ()
  ---------------------------
  word' (x:xs) = sym err' x >> word' xs
  word' _     = return ()


---------------------------
digit :: ErrorMessage -> Parser Char
---------------------------
digit = flip oneOf ['0'..'9']


---------------------------
letter :: ErrorMessage -> Parser Char
---------------------------
letter = flip oneOf ('_' : (['a'..'z'] ++ ['A'..'Z']))

---------------------------
number :: ErrorMessage -> Parser Integer
---------------------------
number err = number' where
  err' = if isJust err then err else mExpectedNumber
  number' = many1 (digit err') >>= return . read


---------------------------
variable :: ErrorMessage -> Parser String
---------------------------
variable err = variable' where
  err' = if isJust err then err else mExpectedNumber
  variable' = letter err' >>= \x -> many0 (letter err' |!| digit err') >>= return . (x:)
