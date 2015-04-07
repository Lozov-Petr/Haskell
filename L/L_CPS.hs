----------------------------------------------------------------------------------------
-- TYPES -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

type Z = Integer
type V = String
type L = String

type State a = a -> Maybe D

data D = Z Z
       | A (State Z)
       | S (State V)

data E = Num Z   | Var V
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
       | Abort
       | Break L
       | Continue L
       | Write  E
       | Read   Elem
       | Assign Elem E
       | Sq     S S
       | IfTE   L E S S
       | While  L E S

data Elem = EV V
          | EA Elem E
          | ES Elem V

data P = Program S

type Input  = [Z]
type Output = [Z]

type C = (State V, Input, Output)

type K = Maybe S

data Gamma = G K (L -> Maybe Gamma) (L -> Maybe Gamma)


----------------------------------------------------------------------------------------
-- INTERPRET ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------

---------------------------
interpretP :: P -> Input -> Maybe Output
---------------------------
interpretP (Program p) i = interpretS (empty, i, []) (G Nothing empty empty) (Just p)  
                       >>= getResult where
    
    ---------------------------                   
    getResult :: C -> Maybe Output
    ---------------------------
    getResult (_,[],o) = Just $ reverse o
    getResult  _       = Nothing


    ---------------------------    
    interpretS :: C -> Gamma -> Maybe S -> Maybe C
    ---------------------------
    interpretS c             (G Nothing _ _)  Nothing           = Just c
    interpretS c             (G k br cn) (Just  Skip)           = interpretS c (G Nothing br cn) k
    interpretS (s,i,o)       (G k br cn) (Just (Write e))       = interpretE i s e
                                                              >>= unZ
                                                              >>= \z -> interpretS (s,i,z:o) (G Nothing br cn) k
    interpretS c             (G k br cn) (Just (Sq l r))        = interpretS c (G (addInK r k) br cn) $ Just l
    interpretS c@(s,i,o)   g@(G k br cn) (Just (IfTE l e a b))  = interpretE i s e
                                                              >>= \d -> ifTEforZ d (interpretS c g' $ Just a) 
                                                                                   (interpretS c g' $ Just b)
                                                                   where k'  = addInK (Break l) k
                                                                         br' = substitution br l $ Just g
                                                                         cn' = substitution cn l Nothing
                                                                         g' = G k' br' cn'
    interpretS c@(s,i,o)   g@(G k br cn) (Just w@(While l e b)) = interpretE i s e
                                                              >>= \d -> ifTEforZ d (interpretS c g' $ Just b)
                                                                                   (interpretS c (G Nothing br cn) k)
                                                                   where kw  = addInK w k
                                                                         k'  = addInK (Continue l) k
                                                                         br' = substitution br l $ Just g
                                                                         cn' = substitution cn l . Just $ G kw br cn
                                                                         g'  = G k' br' cn'

    interpretS c           g@(G _ br _ ) (Just (Break l))       = br l
                                                              >>= \(G k' br' cn') -> interpretS c (G Nothing br' cn') k'

    interpretS c           g@(G _ _  cn) (Just (Continue l))    = cn l
                                                              >>= \(G k' br' cn') -> interpretS c (G Nothing br' cn') k'

    interpretS c@(s,z:i,o)   (G k br cn) (Just (Read ae))       = update i s ae (Z z)
                                                              >>= \s1 -> interpretS (s1,i,o) (G Nothing br cn) k

    interpretS c@(s,i,o)     (G k br cn) (Just (Assign ae e))   = interpretE i s e
                                                              >>= update i s ae
                                                              >>= \s1 -> interpretS (s1,i,o) (G Nothing br cn) k
    interpretS _           _  _                                 = Nothing


    ---------------------------
    update :: Input -> State V -> Elem -> D -> Maybe (State V)
    ---------------------------
    update _ s (EV v)    d = Just $ substitution s v $ Just d
    update i s (ES ae v) d = elemS i s ae
                          >>= unS
                          >>= \struct -> update i s ae (S . substitution struct v $ Just d)
    update i s (EA ae e) d = elemS i s ae
                          >>= unA
                          >>= \array -> interpretE i s e
                          >>= unZ
                          >>= \z -> update i s ae (A . substitution array z $ Just d)


    ---------------------------
    elemS :: Input -> State V -> Elem -> Maybe D
    ---------------------------
    elemS _ s (EV v)    = s v
    elemS i s (ES ae v) = elemS i s ae
                       >>= unS
                       >>= \struct -> struct v
    elemS i s (EA ae e) = elemS i s ae
                       >>= unA
                       >>= \array -> interpretE i s e
                       >>= unZ
                       >>= array

    ---------------------------
    ifTEforZ :: D -> Maybe a -> Maybe a -> Maybe a
    ---------------------------
    ifTEforZ (Z 1) a _ = a
    ifTEforZ (Z 0) _ b = b
    ifTEforZ  _    _ _ = Nothing


    ---------------------------
    addInK :: S -> K -> K
    ---------------------------
    addInK s Nothing  = Just s
    addInK s (Just k) = Just $ Sq s k
    ---------------------------
    unZ :: D -> Maybe Z
    ---------------------------
    unZ (Z z) = Just z
    unZ  _    = Nothing


    ---------------------------
    unS :: D -> Maybe (State V)
    ---------------------------
    unS (S s) = Just s
    unS  _    = Nothing


    ---------------------------
    unA :: D -> Maybe (State Z)
    ---------------------------
    unA (A a) = Just a
    unA  _    = Nothing


    ---------------------------
    substitution :: Eq a => (a -> Maybe b) -> a -> Maybe b -> (a -> Maybe b)
    ---------------------------
    substitution f a1 d = \a2 -> if a1 == a2 then d else f a2 


    ---------------------------
    interpretE :: Input -> State V -> E -> Maybe D
    ---------------------------
    interpretE _  _ (Num n  )     = Just $ Z n
    interpretE _  s (Var v  )     = s v

    interpretE [] _ (EOF    )     = Just $ Z 1
    interpretE _  _ (EOF    )     = Just $ Z 0

    interpretE i  s (Add l r)     = interpretO ((Just .) . (+))     i s l r
    interpretE i  s (Sub l r)     = interpretO ((Just .) . (-))     i s l r
    interpretE i  s (Mul l r)     = interpretO ((Just .) . (*))     i s l r

    interpretE i  s (Eql l r)     = interpretO (boolToMaybeZ (==))  i s l r
    interpretE i  s (NEq l r)     = interpretO (boolToMaybeZ (/=))  i s l r
    interpretE i  s (Grt l r)     = interpretO (boolToMaybeZ (>) )  i s l r
    interpretE i  s (GoE l r)     = interpretO (boolToMaybeZ (>=))  i s l r
    interpretE i  s (Les l r)     = interpretO (boolToMaybeZ (<) )  i s l r
    interpretE i  s (LoE l r)     = interpretO (boolToMaybeZ (<=))  i s l r

    interpretE i  s (Div l r)     = interpretO (funWithoutZero div) i s l r
    interpretE i  s (Mod l r)     = interpretO (funWithoutZero mod) i s l r

    interpretE i  s (And l r)     = interpretO  semAnd              i s l r
    interpretE i  s (Or  l r)     = interpretO  semOr               i s l r

    interpretE i  s (CreateA e)   = interpretE i s e
                                >>= unZ
                                >>= \z -> Just . A $ foldl (\f i -> substitution f i (Just $ Z 0)) empty [0..z-1]

    interpretE i  s (ElemS e v)   = interpretE i s e 
                                >>= unS 
                                >>= \struct -> struct v

    interpretE i  s (ElemA e1 e2) = interpretE i s e1 
                                >>= unA 
                                >>= \array -> interpretE i s e2
                                >>= unZ
                                >>= array


    interpretE i  s (Struct l)    = createState i s l  >>= return . S
    interpretE i  s (Array  l)    = createState i s l0 >>= return . A where
      l0 = zip [0..toInteger (length l) - 1] l
                              
    ---------------------------
    interpretO :: (Z -> Z -> Maybe Z) -> Input -> State V -> E -> E -> Maybe D
    ---------------------------
    interpretO f i s l r = applyO f (interpretE i s l) (interpretE i s r)


    ---------------------------
    applyO :: (Z -> Z -> Maybe Z) -> Maybe D -> Maybe D -> Maybe D
    ---------------------------
    applyO f l r = case (l, r) of
    	                (Just (Z a), Just (Z b)) -> f a b >>= return . Z
                        (Just (A a), Just (A b)) -> return $ A (\z -> applyO f (a z) (b z))
                        (Just (S a), Just (S b)) -> return $ S (\v -> applyO f (a v) (b v))
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


    ---------------------------
    empty :: a -> Maybe b
    ---------------------------
    empty = \_ -> Nothing


    ---------------------------
    updateState :: Eq a => Input -> State V -> Maybe (State a) -> (a, E) -> Maybe (State a)
    ---------------------------
    updateState i s acc (a, e) = acc
                             >>= \f -> interpretE i s e 
                             >>= return . substitution f a . Just


    ---------------------------
    createState :: Eq a => Input -> State V -> [(a, E)] -> Maybe (State a)
    ---------------------------
    createState i s = foldl (updateState i s) $ Just empty

----------------------------------------------------------------------------------------
-- SHOW --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

instance Show Elem where
	show = showElem ""

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
showE _ (EOF)   = "(EOF)"

showE s (ElemS e v)   = "(.)--"  ++  showE (s ++ "|    ")  e  ++ "\n" ++ s ++ "|\n" ++ s ++ "[V]--" ++ v

showE s (ElemA e1 e2) = "([])--" ++  showE (s ++ "|     ") e1 ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e2
showE s (Struct [])         = "(Struct)"
showE s (Struct ((v,e):[])) = "(Struct)--[<-]--[V]--" ++ v ++ "\n" 
                        ++ newS ++ "|\n" ++ newS ++ showE newS e where 
  newS = s ++ "          "
showE s (Struct ((v,e):l )) = "(Struct)--[<-]--[V]--" ++ v ++ "\n" 
                        ++ newS ++ "|\n" ++ newS ++ showE newS e ++ showTail l where 
  newS = s ++ "|         "
  showTail ((v,e):[]) = "\n" ++ s ++ "|\n" ++ s ++ "*--[<-]--[V]--" ++ v ++ "\n" 
                        ++ newS ++ "|\n" ++ newS ++ showE newS e where
    newS = s ++ "   "
  showTail ((v,e):l)  = "\n" ++ s ++ "|\n" ++ s ++ "*--[<-]--[V]--" ++ v ++ "\n" 
                        ++ newS ++ "|\n" ++ newS ++ showE newS e ++ showTail l where
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
showS _ (Abort)          = "[Abort]"
showS _ (Skip)           = "[Skip]"
showS _ (Continue l)     = "[Continue]--<Label>--" ++ l
showS _ (Break l)        = "[Break]--<Label>--" ++ l
showS s (Write e)        = "[Write]--" ++ showE (s ++ "         ") e
showS s (Read ae)        = "[Read]--" ++ showElem (s ++ "        ") ae
showS s (Assign ae e)    = "[:=]--" ++ showElem (s ++ "|     ") ae ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e
showS s (Sq s1 s2)       = "[;]--" ++ showS (s ++ "|    ") s1 ++ "\n" ++ s ++ "|\n" ++ s ++ showS s s2
showS s (IfTE l e t f)   = "[If]--<Label>--" ++ l ++ "\n" ++ s ++ "|\n" ++ s ++
                           "<Cond>--" ++ showE (s ++ "|       ") e ++ "\n" ++ s ++ "|\n" ++ s ++ 
                           "<Then>--" ++ showS (s ++ "|       ") t ++ "\n" ++ s ++ "|\n" ++ s ++ 
                           "<Else>--" ++ showS (s ++ "        ") f
showS s (While l e b)    = "[While]--<Label>--" ++ l ++ "\n" ++ s ++ "|\n" ++ s ++
                           "<Cond>--" ++ showE (s ++ "|       ") e ++ "\n" ++ s ++ "|\n" ++  s ++
                           "<Body>--" ++ showS (s ++ "        ") b

showElem :: String -> Elem -> String
showElem _ (EV v)    = "{V}--" ++ v
showElem s (ES ea v) = "{.}--" ++ showElem (s ++ "|    ") ea ++ "\n" ++ s ++ "|\n" ++ s ++ "{V}--" ++ v
showElem s (EA ea e) = "{[]}--" ++ showElem (s ++ "|     ") ea ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e

----------------------------------------------------------------------------------------
-- TESTS -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------


-- read(number)
-- fact = 1
-- index = 1
-- while (index <= number)
--     fact = fact * index
--     index = index + 1
--     write(fact)

factsP = Program (Sq (Sq (Sq 
                        (Read (EV "number")) 
                        (Assign (EV "fact") (Num 1))) 
                        (Assign (EV "index") (Num 1))) 
                        (While "label" (LoE (Var "index") (Var "number")) 
                          (Sq (Sq
                            (Assign (EV "fact") (Mul (Var "fact") (Var "index"))) 
                            (Assign (EV "index") (Add (Var "index") (Num 1))))
                              (Write (Var "fact")))))

-- read(number)
-- fact = 1
-- index = 1
-- while (1)
--     if (index <= number) break
--     fact = fact * index
--     index = index + 1
--     write(fact)

facts2P = Program (Sq (Sq (Sq 
                        (Read (EV "number")) 
                        (Assign (EV "fact") (Num 1))) 
                        (Assign (EV "index") (Num 1))) 
                        (While "label" (Num 1) 
                          (Sq (Sq (Sq
                            (IfTE "label1" (Grt (Var "index") (Var "number")) (Break "label") (Skip))
                            (Assign (EV "fact") (Mul (Var "fact") (Var "index")))) 
                            (Assign (EV "index") (Add (Var "index") (Num 1))))
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
                      (Read (EV "number"))
                      (Read (EV "fib1")))
                      (Read (EV "fib2")))
                      (IfTE "label" (Grt (Var "number") (Num 0)) 
                          (Write (Var "fib1")) 
                          (Skip)))
                      (IfTE "label" (Grt (Var "number") (Num 1)) 
                          (Write (Var "fib2")) 
                          (Skip)))
                      (Assign (EV "index") (Num 2)))
                      (While "label" (Les (Var "index") (Var "number"))
                        (Sq (Sq (Sq (Sq
                          (Assign (EV "temp") (Var "fib2"))
                          (Assign (EV "fib2") (Add (Var "fib2") (Var "fib1"))))
                          (Assign (EV "fib1") (Var "temp")))
                          (Write (Var "fib2")))
                          (Assign (EV "index") (Add (Var "index") (Num 1)))))) 

--read(number)
--if (number % 10 == 0 || number < 0) abort
--result = 0
--while(number != 0)
--    result = result * 10 + number % 10
--    number = number / 10
--write(result) 

revP = Program (Sq (Sq (Sq (Sq 
                      (Read (EV "number"))
                      (IfTE "label" (Or (Eql (Mod (Var "number") (Num 10)) (Num 0)) 
                      	        (Les (Var "number") (Num 0))) (Abort) (Skip)))
                      (Assign (EV "result") (Num 0))) 
                      (While "label"(NEq (Var "number") (Num 0)) 
                        (Sq 
                          (Assign (EV "result") (Add (Mul (Var "result") (Num 10)) (Mod (Var "number") (Num 10)))) 
                          (Assign (EV "number") (Div (Var "number") (Num 10)))))) 
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
                            (Read (EV "number"))         
                            (IfTE "label" (Les (Var "number") (Num 0)) 
                                (Assign (EV "number") (Sub (Num 0) (Var "number")))
                                (Skip)))
                            (While "label" (GoE (Var "number") (Num 10))
                              (Sq (Sq
                                (Assign (EV "temp") (Num 0))
                                (While "label" (NEq (Var "number") (Num 0))
                                  (Sq 
                                    (Assign (EV "temp") (Add (Var "temp") (Mod (Var "number") (Num 10))))
                                    (Assign (EV "number") (Div (Var "number") (Num 10))))))
                                (Assign (EV "number") (Var "temp")))))
                            (Write (Var "number")))


--A = {X <- 1, Y <- 1 + 1, Z <- {X <- 3, Z <- 4}}
--A.Z.X = A.Z.Z + 22
--write (A.X + A.Y + A.Z.X + A.Z.Z)

structsP = Program (Sq (Sq
              (Assign (EV "A") (Struct [("X", Num 1),
                                        ("Y", Add (Num 1) (Num 1)),
                                        ("Z", Struct [("X", Num 3), 
                                                      ("Z", Num 4)])]))
              (Assign (ES (ES (EV "A") "Z") "X") (Add (ElemS (ElemS (Var "A") "Z") "Z") (Num 22)))) 
              (Write (Add (Add (ElemS (Var "A") "X") 
                               (ElemS (Var "A") "Y")) 
                          (Add (ElemS (ElemS (Var "A") "Z") "X") 
                               (ElemS (ElemS (Var "A") "Z") "Z")))))

--read(N)
--all = {array <- [1,2,1+2], index <- 123, index <- 1, sum <- 0}
--all.array[6 / 2] = N * 2
--while (all.index <= 4)
--   all.sum = all.sum + all.array[all.index - 1]
--   all.index = all.index + 1
--write(all.sum)

arraysP = Program (Sq (Sq (Sq (Sq

        (Read (EV "N"))
        (Assign (EV "all") (Struct [("array", (Array [Num 1, Num 2, Add (Num 1) (Num 2)])), 
                                    ("index", Num 123), 
                                    ("index", Num 1),
                                    ("sum", Num 0)])))
        (Assign (EA (ES (EV "all") "array") (Div (Num 6) (Num 2))) (Mul (Var "N") (Num 2))))

        (While "label" (LoE (ElemS (Var "all") "index") (Num 4)) (Sq
            (Assign (ES (EV "all") "sum") 
               (Add (ElemS (Var "all") "sum") (ElemA (ElemS (Var "all") "array") (Sub (ElemS (Var "all") "index") (Num 1)))))
            (Assign (ES (EV "all") "index") (Add (ElemS (Var "all") "index") (Num 1))))))

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
        (Assign (EV "arr") (Struct [("get", Array []), ("length", Num 0)]))
        (While "label" (Sub (Num 1) EOF) (Sq
            (Read (EA (ES (EV "arr") "get") (ElemS (Var "arr") "length")))
            (Assign (ES (EV "arr") "length") (Add (ElemS (Var "arr") "length") (Num 1))))))
        (Assign (EV "I") (Num 0)))
        (While "label" (Les (Var "I") (Sub (ElemS (Var "arr") "length") (Num 1))) (Sq (Sq
            (Assign (EV "J") (Add (Var "I") (Num 1)))
            (While "label" (Les (Var "J") (ElemS (Var "arr") "length")) (Sq 
                (IfTE "label" (Grt (ElemA (ElemS (Var "arr") "get") (Var "I")) (ElemA (ElemS (Var "arr") "get") (Var "J"))) (Sq (Sq
                    (Assign (EV "temp") (ElemA (ElemS (Var "arr") "get") (Var "I")))
                    (Assign (EA (ES (EV "arr") "get") (Var "I")) (ElemA (ElemS (Var "arr") "get") (Var "J"))))
                    (Assign (EA (ES (EV "arr") "get") (Var "J")) (Var "temp")))

                    (Skip))
                (Assign (EV "J") (Add (Var "J") (Num 1))))))
            (Assign (EV "I") (Add (Var "I") (Num 1))))))
        (Assign (EV "I") (Num 0)))
        (While "label" (Les (Var "I") (ElemS (Var "arr") "length")) (Sq 
            (Write (ElemA (ElemS (Var "arr") "get") (Var "I")))
            (Assign (EV "I") (Add (Var "I") (Num 1))))))


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
              (Read (EV "N")) 
              (Read (EV "M")))
              (Read (EV "K")))
              (Assign (EV "m1") (Array [])))
              (Assign (EV "I") (Num 0)))
              (While "label" (Les (Var "I") (Var "N")) (Sq (Sq (Sq
                  (Assign (EV "J") (Num 0))
                  (Assign (EA (EV "m1") (Var "I")) (Array [])))
                  (While "label" (Les (Var "J") (Var "M")) (Sq
                      (Read (EA (EA (EV "m1") (Var "I")) (Var "J")))
                      (Assign (EV "J") (Add (Var "J") (Num 1))))))
                  (Assign (EV "I") (Add (Var "I") (Num 1))))))
              (Assign (EV "m2") (Array [])))
              (Assign (EV "I") (Num 0)))
              (While "label" (Les (Var "I") (Var "M")) (Sq (Sq (Sq
                  (Assign (EV "J") (Num 0))
                  (Assign (EA (EV "m2") (Var "I")) (Array [])))
                  (While "label" (Les (Var "J") (Var "K")) (Sq
                      (Read (EA (EA (EV "m2") (Var "I")) (Var "J")))
                      (Assign (EV "J") (Add (Var "J") (Num 1))))))
                  (Assign (EV "I") (Add (Var "I") (Num 1))))))
              (Assign (EV "m3") (Array [])))
              (Assign (EV "I") (Num 0)))
              (While "label" (Les (Var "I") (Var "N")) (Sq (Sq (Sq
                  (Assign (EV "J") (Num 0))
                  (Assign (EA (EV "m3") (Var "I")) (CreateA (Var "K"))))
                  (While "label" (Les (Var "J") (Var "K")) (Sq (Sq
                      (Assign (EV "S") (Num 0))
                      (While "label" (Les (Var "S") (Var "M")) (Sq
                          (Assign (EA (EA (EV "m3") (Var "I")) (Var "J")) 
                            (Add  (ElemA (ElemA (Var "m3") (Var "I")) (Var "J")) 
                            (Mul  (ElemA (ElemA (Var "m1") (Var "I")) (Var "S")) 
                                  (ElemA (ElemA (Var "m2") (Var "S")) (Var "J")))))
                          (Assign (EV "S") (Add (Var "S") (Num 1))))))
                      (Assign (EV "J") (Add (Var "J") (Num 1))))))
                  (Assign (EV "I") (Add (Var "I") (Num 1))))))       
              (Assign (EV "I") (Num 0)))
              (While "label" (Les (Var "I") (Var "N")) (Sq (Sq
                  (Assign (EV "J") (Num 0))
                  (While "label" (Les (Var "J") (Var "K")) (Sq
                      (Write (ElemA (ElemA (Var "m3") (Var "I")) (Var "J")))
                      (Assign (EV "J") (Add (Var "J") (Num 1))))))
                  (Assign (EV "I") (Add (Var "I") (Num 1))))))

--arr = {elem = [], length = 0}
--while (1 - eof)
--   read(x)
--   if (x > 0 && x % == 0 || x < 0 && x % 2 == 1)
--       arr.elem[arr.length] = x
--       arr.length = arr.length + 1
--index = 0 
--while (index < arr.length)
--    write(arr.elem[index])
--    index = index + 1

mapP = Program (Sq (Sq (Sq 
          (Assign (EV "arr") (Struct [("elem", Array []), ("length", Num 0)]))
          (While "label" (Sub (Num 1) EOF) (Sq
              (Read (EV "x"))
              (IfTE "label" (Or (And (Grt (Var "x") (Num 0)) (Eql (Mod (Var "x") (Num 2)) (Num 0))) 
                        (And (Les (Var "x") (Num 0)) (Eql (Mod (Var "x") (Num 2)) (Num 1)))) (Sq
                  (Assign (EA (ES (EV "arr") "elem") (ElemS (Var "arr") "length")) (Var "x"))
                  (Assign (ES (EV "arr") "length") (Add (ElemS (Var "arr") "length") (Num 1))))
                  (Skip)))))
          (Assign (EV "index") (Num 0)))
          (While "label" (Les (Var "index") (ElemS (Var "arr") "length")) (Sq
              (Write (ElemA (ElemS (Var "arr") "elem") (Var "index")))
              (Assign (EV "index") (Add (Var "index") (Num 1))))))



test = Program (Sq (Sq
              (Assign (EV "x") (Num 8))
              (While "" (Grt (Var "x") (Num 0)) (Sq (Sq (Sq
                (While "" (NEq (Mod (Var "x") (Num 5)) (Num 0)) (Sq (Sq
                  (Write (Var "x"))
                  (Write (Num $ -3)))
                  (Assign (EV "x") (Sub (Var "x") (Num 1)))))
                (Write (Var "x")))
                (Write (Num $ -2)))
                (Assign (EV "x") (Sub (Var "x") (Num 2))))))
              (Write (Num $ -1)))

