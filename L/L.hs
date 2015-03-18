import System.IO

-- types -------------------------------------------------------------------------------

type Z = Integer 

data D = Z Z 
       | S (V -> Maybe D)
       | A (Z -> Maybe D)

type V = String

data E = Var V   | Num Z
       | Add E E | Sub E E
       | Mul E E | Div E E
       | Eql E E | NEq E E 
       | Grt E E | Les E E
       | GoE E E | LoE E E
       | And E E | Or  E E
       | Mod E E 

       | EOF
       
       | Struct [(V, E)]
       | ElemS  E V

       | Array   [E]
       | ElemA   E E
       | CreateA E

data S = Skip
       | Write  E
       | Read   E
       | Assign E E
       | Sq     S S
       | IfTE   E S S
       | While  E S

data P = Program S

type State = V -> Maybe D

type Input  = [Z]
type Output = [Z]

type C = (State, Input, Output) 

-- parser ------------------------------------------------------------------------------

parser :: String -> P
parser str = Program s where 

  (s, []) = parserS $ lines str

  parserS :: [String] -> (S, [String])
  parserS ("s":  xs ) = (Skip,   xs)
  parserS ("r"  :xs0) = (Read e, xs1)       where (e, xs1) = parserE xs0 
  parserS ("w"  :xs0) = (Write e, xs1)      where (e, xs1) = parserE xs0
  parserS ("="  :xs0) = (Assign e1 e2, xs2) where (e1,xs1) = parserE xs0 
                                                  (e2,xs2) = parserE xs1
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
interpret :: P -> Input -> Maybe Output
---------------------------
interpret (Program p) i = getResult . interpretS p $ (\_ -> Nothing, i, []) where
    
    ---------------------------
    getResult :: Maybe C -> Maybe Output
    ---------------------------    
    getResult (Just (_,[],o)) = Just $ reverse o
    getResult  _              = Nothing

    ---------------------------
    unZ :: D -> Maybe Z
    ---------------------------
    unZ (Z z) = Just z
    unZ  _    = Nothing
 
    ---------------------------
    unS :: D -> Maybe (V -> Maybe D)
    ---------------------------
    unS (S s) = Just s
    unS  _    = Nothing

    ---------------------------
    unA :: D -> Maybe (Z -> Maybe D)
    ---------------------------
    unA (A a) = Just a
    unA  _    = Nothing

    ---------------------------
    ifTEforZ :: D -> Maybe a -> Maybe a -> Maybe a
    ---------------------------
    ifTEforZ (Z 1) a _ = a
    ifTEforZ (Z 0) _ b = b
    ifTEforZ  _    _ _ = Nothing

    ---------------------------
    interpretS :: S -> C -> Maybe C
    ---------------------------
    interpretS  Skip          c         = Just c
    interpretS (Read e)       (s,n:i,o) = interpretS (Assign e (Num n)) (s,i,o)--Just (substitution s v $ Z n, i, o)
    interpretS (Write e)      (s,i,o)   = interpretE i e s  
                                      >>= unZ 
                                      >>= \z -> return (s, i, z:o)  
    
    interpretS (Sq s1 s2)     c1        = interpretS s1 c1 
                                      >>= interpretS s2

    interpretS (IfTE e s1 s2) c@(s,i,o) = interpretE i e s 
                                      >>= \d -> ifTEforZ d (interpretS s1 c) (interpretS s2 c)
    
    interpretS w@(While e s0) c@((s,i,o)) = interpretE i e s 
                                        >>= \d -> ifTEforZ d (interpretS s0 c >>= interpretS w) (Just c)                                                      
    
    interpretS (Assign (Var v) e)        (s,i,o) = interpretE i e s 
                                               >>= \z -> return (substitution s v z, i, o)
    
    interpretS (Assign (ElemS e1 v ) e2) (s,i,o) = interpretE i e2 s 
                                               >>= update i s e1 (Var v) 
                                               >>= \s -> return (s, i, o)
    
    interpretS (Assign (ElemA e1 e2) e3) (s,i,o) = interpretE i e3 s 
                                               >>= \d -> interpretE i e2 s 
                                               >>= unZ 
                                               >>= \z -> update i s e1 (Num z) d
                                               >>= \s -> return (s, i, o)
    
    interpretS _                        _        = Nothing

    ---------------------------
    update :: Input -> State -> E -> E -> D -> Maybe State
    ---------------------------
    update _ s (Var y)        (Var x) v = s y 
                                      >>= unS 
                                      >>= \f -> return . substitution s y . S $ substitution f x v

    update _ s (Var y)        (Num n) v = s y 
                                      >>= unA
                                      >>= \f -> return . substitution s y . A $ substitution f n v
    
    update i s e0@(ElemA e1 e2) (Var x) v = interpretE i e0 s 
                                        >>= unS 
                                        >>= \f -> interpretE i e2 s 
                                        >>= unZ 
                                        >>= \z -> update i s e1 (Num z) . S $ substitution f x v
                                                                                
    update i s e0@(ElemA e1 e2) (Num n) v = interpretE i e0 s 
                                        >>= unA
                                        >>= \f -> interpretE i e2 s 
                                        >>= unZ
                                        >>= \z -> update i s e1 (Num z) . A $ substitution f n v

    update i s e0@(ElemS e y) (Num n) v = interpretE i e0 s 
                                      >>= unA
                                      >>= \f -> update i s e (Var y) . A $ substitution f n v

    update i s e0@(ElemS e y) (Var x) v = interpretE i e0 s 
                                      >>= unS
                                      >>= \f -> update i s e (Var y) . S $ substitution f x v

    ---------------------------    
    substitution :: Eq a => (a -> Maybe D) -> a -> D -> (a -> Maybe D)
    ---------------------------
    substitution s a1 d = \a2 -> if a1 == a2 then Just d else s a2

    ---------------------------
    interpretE :: Input -> E -> State -> Maybe D
    ---------------------------
    interpretE _  (Num z)  _ = Just $ Z z
    interpretE _  (Var v)  s = s v
    interpretE [] (EOF)    _ = Just $ Z 1
    interpretE _  (EOF)    _ = Just $ Z 0 

    interpretE i (Add a b) s = interpretO ((Just .) . (+))     i s a b
    interpretE i (Sub a b) s = interpretO ((Just .) . (-))     i s a b
    interpretE i (Mul a b) s = interpretO ((Just .) . (*))     i s a b

    interpretE i (Eql a b) s = interpretO (boolToMaybeZ (==))  i s a b
    interpretE i (NEq a b) s = interpretO (boolToMaybeZ (/=))  i s a b
    interpretE i (Grt a b) s = interpretO (boolToMaybeZ (>) )  i s a b
    interpretE i (GoE a b) s = interpretO (boolToMaybeZ (>=))  i s a b
    interpretE i (Les a b) s = interpretO (boolToMaybeZ (<) )  i s a b
    interpretE i (LoE a b) s = interpretO (boolToMaybeZ (<=))  i s a b

    interpretE i (Div a b) s = interpretO (funWithoutZero div) i s a b
    interpretE i (Mod a b) s = interpretO (funWithoutZero mod) i s a b

    interpretE i (And a b) s = interpretO  semAnd              i s a b
    interpretE i (Or  a b) s = interpretO  semOr               i s a b

    interpretE i (Struct l) s = foldl update state l >>= return . S where
      update jf (v,e) = jf >>= (\f -> interpretE i e s >>= return . substitution f v)
      state = Just $ \_ -> Nothing

    interpretE i (Array l) s = foldl update state (zip [0..toInteger (length l) - 1] l) >>= Just . A where
      update jf (n,e) = jf >>= (\f -> interpretE i e s >>= return . substitution f n)
      state = Just $ \_ -> Nothing

    interpretE i (ElemS e v) s   = interpretE i e  s 
                               >>= unS 
                               >>= \struct -> struct v

    interpretE i (ElemA e1 e2) s = interpretE i e1 s 
                               >>= unA 
                               >>= \array -> interpretE i e2 s 
                               >>= unZ
                               >>= array

    interpretE i (CreateA e)   s = interpretE i e s >>= (\d -> case d of
                                                        (Z z) -> Just . A $ foldl (\f i -> substitution f i (Z 0)) (\_ -> Nothing) [0..z]
                                                        _     -> Nothing) 
    
    ---------------------------
    interpretO :: (Z -> Z -> Maybe Z) -> Input -> State -> E -> E -> Maybe D
    ---------------------------
    interpretO op i s a b = case (interpretE i a s, interpretE i b s) of
                                 (Just (Z x), Just (Z y)) -> op x y >>= return . Z
                                 _                        -> Nothing


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

showE s (ElemS e v)   = "(.)--"  ++  showE (s ++ "|    ")  e  ++ "\n" ++ s ++ "|\n" ++ s ++ "[V]--" ++ v

showE s (ElemA e1 e2) = "([])--" ++  showE (s ++ "|     ") e1 ++ "\n" ++ s ++ "|\n" ++ s ++ "[V]--" ++ showE (s ++ "     ") e2

showE s (Struct [])         = "(Struct)"
showE s (Struct ((v,e):[])) = "(Struct)--[<-]--[V]--" ++ v ++ "\n" ++ newS ++ "|\n" ++ newS ++ showE newS e where 
  newS = s ++ "          "
showE s (Struct ((v,e):l )) = "(Struct)--[<-]--[V]--" ++ v ++ "\n" ++ newS ++ "|\n" ++ newS ++ showE newS e ++ showTail l where 
  newS = s ++ "|         "
  showTail ((v,e):[]) = "\n" ++ s ++ "|\n" ++ s ++ "*--[<-]--[V]--" ++ v ++ "\n" ++ newS ++ "|\n" ++ newS ++ showE newS e where
    newS = s ++ "   "
  showTail ((v,e):l)  = "\n" ++ s ++ "|\n" ++ s ++ "*--[<-]--[V]--" ++ v ++ "\n" ++ newS ++ "|\n" ++ newS ++ showE newS e ++ showTail l where
    newS = s ++ "|  "

showE s (Array [])        = "(Array)"
showE s (Array (e:[]))    = "(Array)--" ++ showE (s ++ "         ") e
showE s (Array (e:l ))    = "(Array)--" ++ showE (s ++ "|        ") e ++ showTail l where
    showTail (e:[]) = "\n" ++ s ++ "|\n" ++ s ++ "*--" ++ showE (s ++ "   ") e
    showTail (e:l)  = "\n" ++ s ++ "|\n" ++ s ++ "*--" ++ showE (s ++ "|  ") e ++ showTail l

showE s (CreateA e)       = "(CreateA)--" ++ showE (s ++ "           ") e




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
showS _ (Skip)         = "[Skip]"
showS s (Write e)      = "[Write]--" ++ showE (s ++ "         ") e
showS s (Read e)       = "[Read]--" ++ showE (s ++ "        ") e
showS s (Assign e1 e2) = "[:=]--" ++ showE (s ++ "|     ") e1 ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e2
showS s (Sq s1 s2)     = "[;]--" ++ showS (s ++ "|    ") s1 ++ "\n" ++ s ++ "|\n" ++ s ++ showS s s2
showS s (IfTE e t f)   = "[If]--" ++ showE (s ++ "|     ") e ++ "\n" ++ s ++ "|\n" ++ s ++ 
                         "<Then>--" ++ showS (s ++ "|       ") t ++ "\n" ++ s ++ "|\n" ++ s ++ 
                         "<Else>--" ++ showS (s ++ "        ") f
showS s (While e b)    = "[While]--" ++ showE (s ++ "|        ") e ++ "\n" ++ s ++ "|\n" ++ s ++ showS s b

-- main -----------------------------------------------------------------------------------

---------------------------
printInFile :: String -> IO ()
---------------------------
printInFile str = openFile str ReadMode >>= hGetContents >>= putStr . show . parser

---------------------------
interpretInFile :: String -> Input -> IO ()
---------------------------
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
                        (Read (Var "number")) 
                        (Assign (Var "fact") (Num 1))) 
                        (Assign (Var "index") (Num 1))) 
                        (While (LoE (Var "index") (Var "number")) 
                          (Sq (Sq
                            (Assign (Var "fact") (Mul (Var "fact") (Var "index"))) 
                            (Assign (Var "index") (Add (Var "index") (Num 1))))
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
                      (Read (Var "number"))
                      (Read (Var "fib1")))
                      (Read (Var "fib2")))
                      (IfTE (Grt (Var "number") (Num 0)) 
                          (Write (Var "fib1")) 
                          (Skip)))
                      (IfTE (Grt (Var "number") (Num 1)) 
                          (Write (Var "fib2")) 
                          (Skip)))
                      (Assign (Var "index") (Num 2)))
                      (While (Les (Var "index") (Var "number"))
                        (Sq (Sq (Sq (Sq
                          (Assign (Var "temp") (Var "fib2"))
                          (Assign (Var "fib2") (Add (Var "fib2") (Var "fib1"))))
                          (Assign (Var "fib1") (Var "temp")))
                          (Write (Var "fib2")))
                          (Assign (Var "index") (Add (Var "index") (Num 1)))))) 

--read(number)
--result = 0
--while(number != 0)
--    result = result * 10 + number % 10
--    number = number / 10
--write(result) 

revP = Program (Sq (Sq (Sq 
                      (Read (Var "number")) 
                      (Assign (Var "result") (Num 0))) 
                      (While(NEq (Var "number") (Num 0)) 
                        (Sq 
                          (Assign (Var "result") (Add (Mul (Var "result") (Num 10)) (Mod (Var "number") (Num 10)))) 
                          (Assign (Var "number") (Div (Var "number") (Num 10)))))) 
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
                            (Read (Var "number"))         
                            (IfTE (Les (Var "number") (Num 0)) 
                                (Assign (Var "number") (Sub (Num 0) (Var "number")))
                                (Skip)))
                            (While (GoE (Var "number") (Num 10))
                              (Sq (Sq
                                (Assign (Var "temp") (Num 0))
                                (While (NEq (Var "number") (Num 0))
                                  (Sq 
                                    (Assign (Var "temp") (Add (Var "temp") (Mod (Var "number") (Num 10))))
                                    (Assign (Var "number") (Div (Var "number") (Num 10))))))
                                (Assign (Var "number") (Var "temp")))))
                            (Write (Var "number")))


--A = {X <- 1, Y <- 1 + 1, Z <- {X <- 3, Z <- 4}}
--A.Z.X = A.Z.Z + 22
--write (A.X + A.Y + A.Z.X + A.Z.Z)

structsP = Program (Sq (Sq
              (Assign (Var "A") (Struct [("X", Num 1),
                                         ("Y", Add (Num 1) (Num 1)),
                                         ("Z", Struct [("X", Num 3), 
                                                       ("Z", Num 4)])]))
              (Assign (ElemS (ElemS (Var "A") "Z") "X") (Add (ElemS (ElemS (Var "A") "Z") "Z") (Num 22)))) 
              (Write (Add (Add (ElemS (Var "A") "X") (ElemS (Var "A") "Y")) (Add (ElemS (ElemS (Var "A") "Z") "X") (ElemS (ElemS (Var "A") "Z") "Z")))))

--read(N)
--all = {array <- [1,2,1+2], index = 123, index <- 1, sum <- 0}
--all.array[9 / 2] = N * 2
--while (all.index <= 4)
--   all.sum = all.sum + all.array[all.index - 1]
--   all.index = all.index + 1
--write(all.sum)

arraysP = Program (Sq (Sq (Sq (Sq

        (Read (Var "N"))
        (Assign (Var "all") (Struct [("array", (Array [Num 1, Num 2, Add (Num 1) (Num 2)])), 
                                     ("index", Num 123), 
                                     ("index", Num 1),
                                     ("sum", Num 0)])))
        (Assign (ElemA (ElemS (Var "all") "array") (Div (Num 6) (Num 2))) (Mul (Var "N") (Num 2))))

        (While (LoE (ElemS (Var "all") "index") (Num 4)) (Sq
            (Assign (ElemS (Var "all") "sum") 
               (Add (ElemS (Var "all") "sum") (ElemA (ElemS (Var "all") "array") (Sub (ElemS (Var "all") "index") (Num 1)))))
            (Assign (ElemS (Var "all") "index") (Add (ElemS (Var "all") "index") (Num 1))))))

        (Write (ElemS (Var "all") "sum")))


--arr = {get = [], length = 0}
--while (1 - EOF)
--    read(arr.get[arr.length])
--    arr.length = arr.length + 1
--I = 0;
--while (I < arr.length - 1)
--    J = I + 1
--    while (J < arr.length)
--        if (arr.get[I] > arr.get[J])
--            temp = arr.get[I]
--            arr.get[I] = arr.get[J]
--            arr.get[J] = temp
--        J = J + 1
--    I = I + 1
--I = 0
--while (I < arr.length)
--    write(arr.get[I])
--    I = I + 1

sortP = Program (Sq (Sq (Sq (Sq (Sq
        (Assign (Var "arr") (Struct [("get", Array []), ("length", Num 0)]))
        (While (Sub (Num 1) EOF) (Sq
            (Read (ElemA (ElemS (Var "arr") "get") (ElemS (Var "arr") "length")))
            (Assign (ElemS (Var "arr") "length") (Add (ElemS (Var "arr") "length") (Num 1))))))
        (Assign (Var "I") (Num 0)))
        (While (Les (Var "I") (Sub (ElemS (Var "arr") "length") (Num 1))) (Sq (Sq
            (Assign (Var "J") (Add (Var "I") (Num 1)))
            (While (Les (Var "J") (ElemS (Var "arr") "length")) (Sq 
                (IfTE (Grt (ElemA (ElemS (Var "arr") "get") (Var "I")) (ElemA (ElemS (Var "arr") "get") (Var "J"))) (Sq (Sq
                    (Assign (Var "temp") (ElemA (ElemS (Var "arr") "get") (Var "I")))
                    (Assign (ElemA (ElemS (Var "arr") "get") (Var "I")) (ElemA (ElemS (Var "arr") "get") (Var "J"))))
                    (Assign (ElemA (ElemS (Var "arr") "get") (Var "J")) (Var "temp")))

                    (Skip))
                (Assign (Var "J") (Add (Var "J") (Num 1))))))
            (Assign (Var "I") (Add (Var "I") (Num 1))))))
        (Assign (Var "I") (Num 0)))
        (While (Les (Var "I") (ElemS (Var "arr") "length")) (Sq 
            (Write (ElemA (ElemS (Var "arr") "get") (Var "I")))
            (Assign (Var "I") (Add (Var "I") (Num 1))))))


--read(N)
--read(M)
--read(K)
--m1 = []
--I = 0
--while (I < N)
--    J = 0
--    m1[I] = []
--    while (J < M)
--        read(m1[I][J])
--        J = J + 1
--    I = I + 1
--m2 = []        
--I = 0
--while (I < M)
--    J = 0
--    m2[I] = []
--    while (J < K)
--        read(m[I][J])
--        J = J + 1
--    I = I + 1
--m3 = []
--I = 0
--while (I < N)
--    J = 0
--    m3[I] = createA(K)
--    while (J < K)
--        S = 0
--        while (S < M)
--            m3[I][J] = m3[I][J] + m1[I][S] * m2[S][J]
--            S = S + 1
--        J = J + 1
--    I = I + 1
--I = 0
--while (I < N)
--    J = 0
--    while (J < K)
--        write (m3[I][J])
--        J = J + 1
--    I = I + 1

mulMatrP = Program (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq (Sq
              (Read (Var "N")) 
              (Read (Var "M")))
              (Read (Var "K")))
              (Assign (Var "m1") (Array [])))
              (Assign (Var "I") (Num 0)))
              (While (Les (Var "I") (Var "N")) (Sq (Sq (Sq
                  (Assign (Var "J") (Num 0))
                  (Assign (ElemA (Var "m1") (Var "I")) (Array [])))
                  (While (Les (Var "J") (Var "M")) (Sq
                      (Read (ElemA (ElemA (Var "m1") (Var "I")) (Var "J")))
                      (Assign (Var "J") (Add (Var "J") (Num 1))))))
                  (Assign (Var "I") (Add (Var "I") (Num 1))))))
              (Assign (Var "m2") (Array [])))
              (Assign (Var "I") (Num 0)))
              (While (Les (Var "I") (Var "M")) (Sq (Sq (Sq
                  (Assign (Var "J") (Num 0))
                  (Assign (ElemA (Var "m2") (Var "I")) (Array [])))
                  (While (Les (Var "J") (Var "K")) (Sq
                      (Read (ElemA (ElemA (Var "m2") (Var "I")) (Var "J")))
                      (Assign (Var "J") (Add (Var "J") (Num 1))))))
                  (Assign (Var "I") (Add (Var "I") (Num 1))))))
              (Assign (Var "m3") (Array [])))
              (Assign (Var "I") (Num 0)))
              (While (Les (Var "I") (Var "N")) (Sq (Sq (Sq
                  (Assign (Var "J") (Num 0))
                  (Assign (ElemA (Var "m3") (Var "I")) (CreateA (Var "K"))))
                  (While (Les (Var "J") (Var "K")) (Sq (Sq
                      (Assign (Var "S") (Num 0))
                      (While (Les (Var "S") (Var "M")) (Sq
                          (Assign (ElemA (ElemA (Var "m3") (Var "I")) (Var "J")) 
                            (Add  (ElemA (ElemA (Var "m3") (Var "I")) (Var "J")) 
                            (Mul  (ElemA (ElemA (Var "m1") (Var "I")) (Var "S")) 
                                  (ElemA (ElemA (Var "m2") (Var "S")) (Var "J")))))
                          (Assign (Var "S") (Add (Var "S") (Num 1))))))
                      (Assign (Var "J") (Add (Var "J") (Num 1))))))
                  (Assign (Var "I") (Add (Var "I") (Num 1))))))       
              (Assign (Var "I") (Num 0)))
              (While (Les (Var "I") (Var "N")) (Sq (Sq
                  (Assign (Var "J") (Num 0))
                  (While (Les (Var "J") (Var "K")) (Sq
                      (Write (ElemA (ElemA (Var "m3") (Var "I")) (Var "J")))
                      (Assign (Var "J") (Add (Var "J") (Num 1))))))
                  (Assign (Var "I") (Add (Var "I") (Num 1))))))

