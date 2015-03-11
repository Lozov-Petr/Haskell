data Tree = E Tree Tree 
          | V String 
          | C Bool deriving Eq

-- Вывод --------------------------------------------------------------------------------------
instance Show Tree where
  show tree = "\n   " ++ printTree "   " tree ++ "\n" where
    printTree _   (V s)   = s
    printTree _   (C b)   = show b
    printTree str (E l r) = 
       "+--" ++ printTree (str ++ "|  ") r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l


-- Нормализация -------------------------------------------------------------------------------
normalize t@(V v)   = E t $ C True
normalize (E l r) = add (normalize l) $ normalize r where
    
    add   (C lb)    (C rb)    = C $ lb == rb
    add l@(C _)     (E rl rr) = E rl $ add l rr
    add   (E ll lr) (E rl rr) = E u . add (union l lr) $ union r rr where
        
        (u, l, r) = join ll rl

        join l@(V lv)       r@(V rv)       | lv == rv = (E l r, Nothing, Nothing)
                                           | lv <  rv = (l    , Nothing, Just r )
                                           | True     = (r    , Just l , Nothing)
        join l@(V lv)       r@(E (V rv) _) | lv == rv = (l    , Nothing, Nothing)
                                           | lv <  rv = (l    , Nothing, Just r )
                                           | True     = (r    , Just l , Nothing)
        join l@(E (V lv) _) r@(E (V rv) _) | lv == rv = (l    , Nothing, Nothing)
                                           | lv <  rv = (l    , Nothing, Just r )
                                           | True     = (r    , Just l , Nothing)
        join l              r                         = let (a, b, c) = join r l in (a, c, b) 

        
        union  Nothing t = t
        union (Just l) r = E l r

    add l r = add r l

normalize t = t

-- Эквивалентность -----------------------------------------------------------------------------
infix 5 ~~
a ~~ b = (normalize a) == (normalize b)

-- Все деревья ---------------------------------------------------------------------------------
allTrees = generate [] startTrees where

  startTrees = C True : C False : [V [v] | v <- ['a','b']]

  generate visTrees unvisTrees = unvisTrees ++ generate (visTrees ++ unvisTrees) newTrees where

    newTrees =  create   visTrees unvisTrees 
             ++ create unvisTrees   visTrees
             ++ create unvisTrees unvisTrees
    
    create tsl tsr = [E tl tr | tl <- tsl, tr <- tsr] 
    

-- Тесты ---------------------------------------------------------------------------------------
classes []     = []
classes (x:xs) = oneClass : classes other where
    (oneClass, other) = getClass x xs [x] []

    getClass _ []     accT accF = (accT, accF)
    getClass x (l:ls) accT accF | x ~~ l = getClass x ls (l:accT) accF
                                | True   = getClass x ls  accT    (l:accF)

