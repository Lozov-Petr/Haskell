module Interpret where

import Types

----------------------------------------------------------------------------------------
-- INTERPRET ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------

---------------------------
interpretP :: P -> Input -> Maybe Output
---------------------------
interpretP (Program p) i = interpretS (empty, i, []) (G Nothing empty empty empty) (Just p)  
                       >>= getResult where
    
    ---------------------------                   
    getResult :: C -> Maybe Output
    ---------------------------
    getResult (_,[],o) = Just $ reverse o
    getResult  _       = Nothing


    ---------------------------    
    interpretS :: C -> Gamma -> Maybe S -> Maybe C
    ---------------------------
    interpretS c             (G Nothing _ _ _)  Nothing           = Just c
    interpretS c             (G k br cn t) (Just  Skip)           = interpretS c (G Nothing br cn t) k
    interpretS (s,i,o)       (G k br cn t) (Just (Write e))       = interpretE i s e
                                                                >>= unZ
                                                                >>= \z -> interpretS (s,i,z:o) (G Nothing br cn t) k
    interpretS c             (G k br cn t) (Just (Sq l r))        = interpretS c (G (addInK r k) br cn t) $ Just l
    interpretS c@(s,i,_)   g@(G k br cn t) (Just (IfTE l e a b))  = interpretE i s e
                                                                >>= \d -> ifTEforZ d (interpretS c g' $ Just a) 
                                                                                     (interpretS c g' $ Just b)
                                                                     where k'  = Just $ Break l
                                                                           br' = substitution br l $ Just g
                                                                           cn' = substitution cn l Nothing
                                                                           g'  = G k' br' cn' t
    interpretS c@(s,i,_)   g@(G k br cn t) (Just w@(While l e b)) = interpretE i s e
                                                                >>= \d -> ifTEforZ d (interpretS c g' $ Just b)
                                                                                     (interpretS c (G Nothing br cn t) k)
                                                                     where kw  = addInK w k
                                                                           k'  = Just $ Continue l
                                                                           br' = substitution br l $ Just g
                                                                           cn' = substitution cn l . Just $ G kw br cn t
                                                                           g'  = G k' br' cn' t

    interpretS c             (G _ br' _ _) (Just (Break l))       = br' l
                                                                >>= \(G k br cn t) -> interpretS c (G Nothing br cn t) k

    interpretS c             (G _ _ cn' _) (Just (Continue l))    = cn' l
                                                                >>= \(G k br cn t) -> interpretS c (G Nothing br cn t) k

    interpretS c@(s,z:i,o)   (G k br cn t) (Just (Read ae))       = update i s ae (Z z)
                                                                >>= \s1 -> interpretS (s1,i,o) (G Nothing br cn t) k

    interpretS c@(s,i,o)     (G k br cn t) (Just (Assign ae e))   = interpretE i s e
                                                                >>= update i s ae
                                                                >>= \s1 -> interpretS (s1,i,o) (G Nothing br cn t) k

    interpretS c@(s,i,_)   g@(G k br cn t) (Just (Try l tr e ct)) = interpretE i s e
                                                                >>= unZ
                                                                >>= \z -> interpretS c (create z) (Just tr)
                                                                     where create z = (G kt br' cn' t') where
                                                                                kt  = Just $ Break l
                                                                                br' = substitution br l $ Just g
                                                                                cn' = substitution cn l Nothing
                                                                                k'  = addInK ct kt 
                                                                                t'  = substitution t z . Just $ G k' br' cn' t

    interpretS c@(s,i,_)     (G _ _ _  t') (Just (Throw e))       = interpretE i s e
                                                                >>= unZ
                                                                >>= t'
                                                                >>= \(G k br cn t) -> interpretS c (G Nothing br cn t) k   

    
    interpretS _           _  _                                   = Nothing


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

    interpretE i  s (Not e  )     = interpretE i s e 
                                >>= unZ 
                                >>= semNot
                                >>= Just . Z

    interpretE i  s (Inv e  )     = interpretE i s e
                                >>= unZ
                                >>= Just . Z . (0-)

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
    semNot :: Z -> Maybe Z
    ---------------------------
    semNot 0 = Just 1
    semNot 1 = Just 0


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