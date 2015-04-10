import Types
import Interpret

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
--while (!EOF)
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
        (While "label" (Not (Add EOF (Num 0))) (Sq
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
--while (!EOF)
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
          (While "label" (Not EOF) (Sq
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




test = Program (Sq
      (Try "1"
        (Try "1" (Sq
          (Try "1" 
            (Break "1")
          (Num 3)
            (Skip))
          (Throw (Num 1)))
        (Num 2)
          (Skip))
        (Num 1)
          (Write (Num 1)))
        (Write (Num 1)))

