data Reg a = Empty 
           | Concat (Reg a) (Reg a)
           | Union (Reg a) (Reg a)
           | Word [a]
           | Star (Reg a) deriving Eq

instance Show a => Show (Reg a) where
  show (Word w)     = show w
  show (Empty )     = "[EMPTY]"
  show (Union a b)  = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (Concat a b) = "(" ++ show a ++ show b ++ ")"
  show (Star a)     = "(" ++ show a ++ "*)"

delta :: Reg a -> Bool
delta (Word [])     = True
delta (Word _)      = False
delta Empty         = False
delta (Union a b)   = delta a || delta b 
delta (Concat a b)  = delta a && delta b
delta (Star _)      = True

union Empty a              = a
union a     Empty          = a
union a     b     | a == b = a
union a     b              = Union a b

conc Empty     _         = Empty
conc _         Empty     = Empty
conc (Word []) a         = a
conc a         (Word []) = a
conc a b                  = Concat a b

star (Star a)  = Star a
star Empty     = Empty
star (Word []) = Word []
star a         = Star a

normalize (Union a b)  = union (normalize a) (normalize b)
normalize (Concat a b) = conc (normalize a) (normalize b)
normalize (Star a)     = star (normalize a)
normalize a            = a

derivative :: Eq a => Reg a -> a -> Reg a
derivative Empty         _           = Empty
derivative (Word (x:xs)) c | x == c  = Word xs
derivative (Word _)      _           = Empty
derivative (Union a b)   c           = union (derivative a c) (derivative b c)
derivative (Concat a b)  c | delta a = union (conc (derivative a c) b) (derivative b c)
derivative (Concat a b)  c           = conc (derivative a c) b
derivative (Star a)      c           = conc (derivative a c) (star a)

derivatives :: Eq a => Reg a -> [a] -> Reg a
derivatives r = foldl derivative (normalize r) 

parse ::Eq a => Reg a -> [a] -> Bool
parse r w = delta $ foldl derivative r w  

reg = Union (Star (Word "00000")) (Star (Word "0000000"))

main = print $ (derivatives reg "000000000000000")
