import System.IO

-- types -------------------------------------------------------------------------------

type Z = Integer 

type D = Z

type V = String

data E = Var V   | Num D
       | Add E E | Sub E E
       | Mul E E | Div E E
       | Eql E E | NEq E E 
       | Grt E E | Les E E
       | GoE E E | LoE E E
       | And E E | Or  E E
       | Mod E E | EOF

data S = Skip
       | Write  E
       | Read   V
       | Assign V E
       | Sq     S S
       | IfTE   E S S
       | While  E S

data P = Program S

type State = V -> Maybe Z

type C = (State, [Z], [Z]) 

-- parser ------------------------------------------------------------------------------

parser :: String -> P
parser str = Program s where 

  (s, []) = parserS $ lines str

  parserS :: [String] -> (S, [String])
  parserS ("s":  xs ) = (Skip,   xs)
  parserS ("r":x:xs ) = (Read x, xs)
  parserS ("w":  xs0) = (Write e, xs1)      where (e, xs1) = parserE xs0
  parserS ("=":x:xs0) = (Assign x e, xs1)   where (e, xs1) = parserE xs0 
  parserS (";":  xs0) = (Sq s1 s2, xs2)     where (s1,xs1) = parserS xs0
                                                  (s2,xs2) = parserS xs1 
  parserS ("i":  xs0) = (IfTE e s1 s2, xs3) where (e, xs1) = parserE xs0
                                                  (s1,xs2) = parserS xs1
                                                  (s2,xs3) = parserS xs2
  parserS ("l":  xs0) = (While e s, xs2)    where (e, xs1) = parserE xs0
                                                  (s, xs2) = parserS xs1

  parserE :: [String] -> (E, [String])
  parserE ("e":  xs ) = (EOF,          xs)
  parserE ("x":x:xs ) = (Var x,        xs)
  parserE ("c":x:xs ) = (Num $ read x, xs)
  parserE ("*":x:xs0) = (op l r, xs2) where 
    (l, xs1) = parserE xs0
    (r, xs2) = parserE xs1
    op = case x of {"+"  -> Add; "-"  -> Sub; "*"  -> Mul; "/"  -> Div; "%"  -> Mod; 
                    "==" -> Eql; "!=" -> NEq; ">"  -> Grt; ">=" -> GoE; "<"  -> Les; "<=" -> LoE;
                    "&&" -> And; "||" -> Or ;}


-- interpret ---------------------------------------------------------------------------

---------------------------
interpret :: P -> [Z] -> Maybe [Z]
---------------------------
interpret (Program p) i = getResult . interpretS p $ Just (\_ -> Nothing, i, []) where
    
    ---------------------------
    getResult :: Maybe C -> Maybe [Z]
    ---------------------------    
    getResult (Just (_,[],o)) = Just $ reverse o
    getResult  _              = Nothing

    ---------------------------
    interpretS :: S -> Maybe C -> Maybe C
    ---------------------------
    interpretS  Skip            c                = c
    interpretS (Read v)         (Just (s,n:i,o)) = Just (substitution s v n, i, o)
    interpretS (Write e)        (Just (s,i,o))   = interpretE i e s >>= (\z -> return (s, i, z:o))
    interpretS (Assign v e)     (Just (s,i,o))   = interpretE i e s >>= (\z -> return (substitution s v z, i, o))
    interpretS (Sq s1 s2)       c1               = interpretS s2 $ interpretS s1 c1
    interpretS (IfTE e s1 s2)   c@(Just (s,i,o)) = interpretE i e s >>= (\z -> case z of
                                                                                  1 -> interpretS s1 c
                                                                                  0 -> interpretS s2 c
                                                                                  _ -> Nothing)
    interpretS w@(While e s0)   c@(Just (s,i,o)) = interpretE i e s >>= (\z -> case z of
                                                                                  1 -> interpretS w $ interpretS s0 c
                                                                                  0 -> c
                                                                                  _ -> Nothing)
    interpretS _                _                = Nothing

    ---------------------------
    substitution :: State -> V -> Z -> State
    ---------------------------
    substitution s v1 z = \v2 -> if v1 == v2 then Just z else s v2

    ---------------------------
    interpretE :: [Z] -> E -> State -> Maybe Z
    ---------------------------
    interpretE _  (Num z)  _ = Just z
    interpretE _  (Var v)  s = s v
    interpretE [] (EOF)    _ = Just 1
    interpretE _  (EOF)    _ = Just 0 

    interpretE i  (Add a b) s = interpretO ((Just .) . (+))     i s a b
    interpretE i  (Sub a b) s = interpretO ((Just .) . (-))     i s a b
    interpretE i  (Mul a b) s = interpretO ((Just .) . (*))     i s a b

    interpretE i  (Eql a b) s = interpretO (boolToMaybeZ (==))  i s a b
    interpretE i  (NEq a b) s = interpretO (boolToMaybeZ (/=))  i s a b
    interpretE i  (Grt a b) s = interpretO (boolToMaybeZ (>) )  i s a b
    interpretE i  (GoE a b) s = interpretO (boolToMaybeZ (>=))  i s a b
    interpretE i  (Les a b) s = interpretO (boolToMaybeZ (<) )  i s a b
    interpretE i  (LoE a b) s = interpretO (boolToMaybeZ (<=))  i s a b

    interpretE i  (Div a b) s = interpretO (funWithoutZero div) i s a b
    interpretE i  (Mod a b) s = interpretO (funWithoutZero mod) i s a b

    interpretE i  (And a b) s = interpretO  semAnd              i s a b
    interpretE i  (Or  a b) s = interpretO  semOr               i s a b
    
    ---------------------------
    interpretO :: (Z -> Z -> Maybe Z) -> [Z] -> State -> E -> E -> Maybe Z
    ---------------------------
    interpretO op i s a b = interpretE i a s >>= (\x -> interpretE i b s >>= op x) 

    ---------------------------
    boolToMaybeZ :: (Z -> Z -> Bool) -> Z -> Z -> Maybe Z
    ---------------------------
    boolToMaybeZ f a b = Just $ if f a b then 1 else 0

    ---------------------------
    funWithoutZero :: (Z -> Z -> Z) -> Z -> Z -> Maybe Z
    ---------------------------
    funWithoutZero _ _ 0 = Nothing
    funWithoutZero f a b = Just $ f a b

    ---------------------------
    semAnd :: Z -> Z -> Maybe Z
    ---------------------------
    semAnd 0 0 = Just 0
    semAnd 0 1 = Just 0
    semAnd 1 0 = Just 0
    semAnd 1 1 = Just 1
    semAnd _ _ = Nothing

    ---------------------------
    semOr  :: Z -> Z -> Maybe Z
    ---------------------------
    semOr  0 0 = Just 0
    semOr  0 1 = Just 1
    semOr  1 0 = Just 1
    semOr  1 1 = Just 1
    semOr  _ _ = Nothing


-- Show ----------------------------------------------------------------------------------

instance Show E where 
  show = showE ""

instance Show S where
	show = showS ""

instance Show P where
	show (Program p) = "Program:\n\n    " ++ showS "    " p ++ "\n" 


---------------------------
showE  :: String -> E -> String
---------------------------
showE _ (Num a) = "(N)--" ++ show a
showE _ (Var s) = "(V)--" ++ s
showE _ (EOF)   = "EOF"
showE s x = case x of
          Mul l r -> showOp "*"  l r
          Div l r -> showOp "/"  l r
          Add l r -> showOp "+"  l r
          Sub l r -> showOp "-"  l r
          Les l r -> showOp "<"  l r
          LoE l r -> showOp "<=" l r
          Eql l r -> showOp "==" l r
          NEq l r -> showOp "!=" l r
          GoE l r -> showOp ">=" l r
          Grt l r -> showOp ">"  l r
          Mod l r -> showOp "%"  l r
          Or  l r -> showOp "||" l r
          And l r -> showOp "&&" l r
  where
    showOp op l r = "(" ++ op ++ ")--" ++ showE newStr l ++ "\n" ++ s ++ "|\n" ++ s ++ showE s r where
      newStr = s ++ "|" ++ map (\_ -> ' ') [1..length op + 3]


---------------------------
showS  :: String -> S -> String
---------------------------
showS _ (Skip)       = "[Skip]"
showS s (Write e)    = "[Write]--" ++ showE (s ++ "         ") e
showS _ (Read v)     = "[Read]--" ++ "[V]--" ++ v
showS s (Assign v e) = "[:=]--" ++ "[V]--" ++ v ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e
showS s (Sq s1 s2)   = "[;]--" ++ showS (s ++ "|    ") s1 ++ "\n" ++ s ++ "|\n" ++ s ++ showS s s2
showS s (IfTE e t f) = "[If]--" ++ showE (s ++ "|     ") e ++ "\n" ++ s ++ "|\n" ++ s ++ 
                       "<Then>--" ++ showS (s ++ "|       ") t ++ "\n" ++ s ++ "|\n" ++ s ++ 
                       "<Else>--" ++ showS (s ++ "        ") f
showS s (While e b)  = "[While]--" ++ showE (s ++ "|        ") e ++ "\n" ++ s ++ "|\n" ++ s ++ showS s b

-- main -----------------------------------------------------------------------------------

---------------------------
printInFile :: String -> IO ()
---------------------------
printInFile str = openFile str ReadMode >>= hGetContents >>= putStr . show . parser

interpretInFile :: String -> [Z] -> IO ()
interpretInFile str i = openFile str ReadMode >>= hGetContents >>= putStr . show . (\p -> interpret p i) . parser


-- Tests ----------------------------------------------------------------------------------

-- read(number)
-- fact = 1
-- index = 1
-- while (index <= number)
--     fact = fact * index
--     index = index + 1
--     write(fact)

factsP = Program (Sq (Sq (Sq 
                        (Read "number") 
                        (Assign "fact" (Num 1))) 
                        (Assign "index" (Num 1))) 
                        (While (LoE (Var "index") (Var "number")) 
                          (Sq (Sq
                            (Assign "fact" (Mul (Var "fact") (Var "index"))) 
                            (Assign "index" (Add (Var "index") (Num 1))))
                              (Write (Var "fact")))))

--read(number)
--read(fib1)
--read(fib2)
--if (number > 0) write(fib1)
--if (number > 1) write(fib2)
--index = 2
--while (index < number)
--    temp = fib2
--    fib2 = fib2 + fib1
--    fib1 = temp
--    write(fib2)
--    index = index + 1

fibsP = Program (Sq (Sq (Sq (Sq (Sq (Sq 
                      (Read "number")
                      (Read "fib1"))
                      (Read "fib2"))
                      (IfTE (Grt (Var "number") (Num 0)) 
                          (Write (Var "fib1")) 
                          (Skip)))
                      (IfTE (Grt (Var "number") (Num 1)) 
                          (Write (Var "fib2")) 
                          (Skip)))
                      (Assign "index" (Num 2)))
                      (While (Les (Var "index") (Var "number"))
                        (Sq (Sq (Sq (Sq
                          (Assign "temp" (Var "fib2"))
                          (Assign "fib2" (Add (Var "fib2") (Var "fib1"))))
                          (Assign "fib1" (Var "temp")))
                          (Write (Var "fib2")))
                          (Assign "index" (Add (Var "index") (Num 1)))))) 

--read(number)
--result = 0
--while(number != 0)
--    result = result * 10 + number % 10
--    number = number / 10
--write(result) 

revP = Program (Sq (Sq (Sq 
                      (Read "number") 
                      (Assign "result" (Num 0))) 
                      (While(NEq (Var "number") (Num 0)) 
                        (Sq 
                          (Assign "result" (Add (Mul (Var "result") (Num 10)) (Mod (Var "number") (Num 10)))) 
                          (Assign "number" (Div (Var "number") (Num 10)))))) 
                      (Write (Var "result")))


--read(number)
--if (number < 0) number = 0 - number
--while (number >= 10)
--    temp = 0
--    while (number != 0)
--        temp = temp + number % 10
--        number = number / 10
--    number = temp
--write(number)

sumDigitP = Program (Sq (Sq (Sq 
                            (Read "number")         
                            (IfTE (Les (Var "number") (Num 0)) 
                                (Assign "number" (Sub (Num 0) (Var "number")))
                                (Skip)))
                            (While (GoE (Var "number") (Num 10))
                              (Sq (Sq
                                (Assign "temp" (Num 0))
                                (While (NEq (Var "number") (Num 0))
                                  (Sq 
                                    (Assign "temp" (Add (Var "temp") (Mod (Var "number") (Num 10))))
                                    (Assign "number" (Div (Var "number") (Num 10))))))
                                (Assign "number" (Var "temp")))))
                            (Write (Var "number")))
