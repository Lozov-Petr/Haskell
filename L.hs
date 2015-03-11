type Z = Integer 

type V = String

data E = Var V   | Num Z
       | Add E E | Sub E E
       | Mul E E | Div E E
       | Eql E E | NEq E E 
       | Grt E E | Les E E
       | GoE E E | LoE E E
       | And E E | Or  E E
       | Mod E E 

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
    interpretS (Write e)        (Just (s,i,o))   = interpretE e s >>= (\z -> return (s, i, z:o))
    interpretS (Assign v e)     (Just (s,i,o))   = interpretE e s >>= (\z -> return (substitution s v z, i, o))
    interpretS (Sq s1 s2)       c1               = interpretS s2 $ interpretS s1 c1
    interpretS (IfTE e s1 s2)   c@(Just (s,i,o)) = interpretE e s >>= (\z -> case z of
                                                                                  1 -> interpretS s1 c
                                                                                  0 -> interpretS s2 c
                                                                                  _ -> Nothing)
    interpretS w@(While e s0)   c@(Just (s,i,o)) = interpretE e s >>= (\z -> case z of
                                                                                  1 -> interpretS w $ interpretS s0 c
                                                                                  0 -> c
                                                                                  _ -> Nothing)
    interpretS _                _                = Nothing

    ---------------------------
    substitution :: State -> V -> Z -> State
    ---------------------------
    substitution s v1 z = \v2 -> if v1 == v2 then Just z else s v2

    ---------------------------
    interpretE :: E -> State -> Maybe Z
    ---------------------------
    interpretE (Num z)   _ = Just z
    interpretE (Var v)   s = s v

    interpretE (Add a b) s = interpretO ((Just .) . (+))     s a b
    interpretE (Sub a b) s = interpretO ((Just .) . (-))     s a b
    interpretE (Mul a b) s = interpretO ((Just .) . (*))     s a b

    interpretE (Eql a b) s = interpretO (boolToMaybeZ (==))  s a b
    interpretE (NEq a b) s = interpretO (boolToMaybeZ (/=))  s a b
    interpretE (Grt a b) s = interpretO (boolToMaybeZ (>) )  s a b
    interpretE (GoE a b) s = interpretO (boolToMaybeZ (>=))  s a b
    interpretE (Les a b) s = interpretO (boolToMaybeZ (<) )  s a b
    interpretE (LoE a b) s = interpretO (boolToMaybeZ (<=))  s a b

    interpretE (Div a b) s = interpretO (funWithoutZero div) s a b
    interpretE (Mod a b) s = interpretO (funWithoutZero mod) s a b
    interpretE (And a b) s = interpretO  semAnd              s a b
    interpretE (Or  a b) s = interpretO  semOr               s a b
    
    ---------------------------
    interpretO :: (Z -> Z -> Maybe Z) -> State -> E -> E -> Maybe Z
    ---------------------------
    interpretO op s a b = interpretE a s >>= (\x -> interpretE b s >>= op x) 

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
                       "<T>--" ++ showS (s ++ "|    ") t ++ "\n" ++ s ++ "|\n" ++ s ++ 
                       "<F>--" ++ showS (s ++ "     ") f
showS s (While e b)  = "[While]--" ++ showE (s ++ "|        ") e ++ "\n" ++ s ++ "|\n" ++ s ++ showS s b


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
                      (IfTE (Grt (Var "number") (Num 0)) (Write (Var "fib1")) (Skip)))
                      (IfTE (Grt (Var "number") (Num 1)) (Write (Var "fib2")) (Skip)))
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