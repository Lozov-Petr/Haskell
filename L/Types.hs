module Types where

----------------------------------------------------------------------------------------
-- TYPES FOR INTERPRETER ---------------------------------------------------------------
----------------------------------------------------------------------------------------

type Z = Integer
type V = String
type L = String

type State a = a -> Maybe D

data D = Z Z
       | A (State Z)
       | S (State V)

data E = Num Z   | Var V

       | Not E   | Inv E
       | Add E E | Sub E E
       | Mul E E | Div E E
       | Eql E E | NEq E E 
       | Grt E E | Les E E
       | GoE E E | LoE E E
       | And E E | Or  E E
       | Mod E E | Pow E E

       | If E E E

       | EOF
       
       | Struct [(V, E)]
       | ElemS  E V

       | Array   [E]
       | ElemA   E E
       | CreateA E

data S = Skip
       | Abort
       | Break    L
       | Continue L
       | Write    E
       | Read     E
       | Assign   E E
       | Sq       S S
       | IfTE     E S S
       | While    L E S
       | Repeat   L S E
       | For      L V E E E S
       | Try      S E S
       | Throw    E
       | Switch   E [(E,S)] S

data P = Program S

type Input  = [Z]
type Output = [Z]

type C = (State V, Input, Output)

type K = Maybe S

data Gamma = G K (L -> Maybe Gamma) (L -> Maybe Gamma) (Z -> Maybe Gamma)

data Associativity = LeftAssoc | RightAssoc | NotAssoc deriving Eq

----------------------------------------------------------------------------------------
-- SHOW --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

instance Show E where 
  show = showE ""

instance Show S where
      show = showS ""

instance Show P where
      show (Program p) = "Tree:\n\n    " ++ showS "    " p ++ "\n" 


---------------------------
newLine :: String -> String
---------------------------
newLine s = "\n" ++ s ++ "|\n" ++ s


---------------------------
showE  :: String -> E -> String
---------------------------
showE _ (Num a) = "(N)--" ++ show a
showE _ (Var s) = "(V)--" ++ s
showE _ (EOF)   = "(EOF)"

showE s (Not e) = "(!)--" ++ showE (s ++ "     ") e
showE s (Inv e) = "(~)--" ++ showE (s ++ "     ") e

showE s (ElemS e v)   = "(.)--"  ++  showE (s ++ "|    ")  e  ++ newLine s ++ "[V]--" ++ v

showE s (ElemA e1 e2) = "([])--" ++  showE (s ++ "|     ") e1 ++ newLine s ++ showE s e2
showE s (Struct [])         = "(Struct)"
showE s (Struct ((v,e):[])) = "(Struct)--*--[:=]--[V]--" ++ v ++ newLine newS 
                              ++ showE newS e where newS = s ++ "             "
showE s (Struct ((v,e):l )) = "(Struct)--*--[:=]--[V]--" ++ v ++ newLine newS ++ showE newS e 
                              ++ showTail l where 
  newS = s ++ "|            "
  showTail ((v,e):[]) = "\n" ++ s ++ "|\n" ++ s ++ "*--[:=]--[V]--" ++ v ++ newLine newS 
                              ++ showE newS e where newS = s ++ "   "
  showTail ((v,e):l)  = "\n" ++ s ++ "|\n" ++ s ++ "*--[:=]--[V]--" ++ v ++ newLine newS 
                              ++ showE newS e ++ showTail l where newS = s ++ "|  "

showE s (Array [])     = "(Array)"
showE s (Array (e:[])) = "(Array)--*--" ++ showE (s ++ "            ") e
showE s (Array (e:l )) = "(Array)--*--" ++ showE (s ++ "|           ") e ++ showTail l where
    showTail (e:[]) = "\n" ++ s ++ "|\n" ++ s ++ "*--" ++ showE (s ++ "   ") e
    showTail (e:l)  = "\n" ++ s ++ "|\n" ++ s ++ "*--" ++ showE (s ++ "|  ") e ++ showTail l

showE s (CreateA e) = "(CreateArray)--" ++ showE (s ++ "               ") e

showE s (If c t f) = "(if)--<cond>--" ++ showE (s ++ "|             ") c ++ newLine s ++
                     "<?>--" ++ showE (s ++ "|    ") t ++ newLine s ++
                     "<:>--" ++ showE (s ++ "     ") f


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
          Pow l r -> showOp "^"  l r
  where
    showOp op l r = "(" ++ op ++ ")--" ++ showE newStr l ++ newLine s ++ showE s r where
      newStr = s ++ "|" ++ map (\_ -> ' ') [1..length op + 3]


---------------------------
showS  :: String -> S -> String
---------------------------
showS _ (Abort)           = "[Abort]"
showS _ (Skip)            = "[Skip]"
showS _ (Continue l)      = "[Continue]--<Label>--" ++ l
showS _ (Break l)         = "[Break]--<Label>--" ++ l
showS s (Throw e)         = "[Throw]--" ++ showE (s ++ "         ") e
showS s (Write e)         = "[Write]--" ++ showE (s ++ "         ") e
showS s (Read e)          = "[Read]--" ++ showE (s ++ "        ") e
showS s (Assign e1 e2)    = "[:=]--" ++ showE (s ++ "|     ") e1 ++ newLine s ++ showE s e2
showS s (Sq s1 s2)        = "[;]--" ++ showS (s ++ "|    ") s1 ++ newLine s ++ showS s s2
showS s (IfTE e t f)      = "[If]--<Cond>--" ++ showE (s ++ "|              ") e ++ newLine s ++ 
                            "<Then>--" ++ showS (s ++ "|       ") t ++ newLine s ++ 
                            "<Else>--" ++ showS (s ++ "        ") f
showS s (While l e b)     = "[While]--<Label>--" ++ l ++ newLine s ++
                            "<Cond>--" ++ showE (s ++ "|       ") e ++ newLine s ++
                            "<Body>--" ++ showS (s ++ "        ") b
showS s (Repeat l b e)    = "[Repeat]--<Label>--" ++ l ++ newLine s ++
                            "<Body>--" ++ showS (s ++ "|       ") b ++ newLine s ++
                            "<Cond>--" ++ showE (s ++ "        ") e
showS s (For l v x y z b) = "[For]--<Label>--" ++ l ++ newLine s ++
                            "<Counter>--" ++ v ++ newLine s ++
                            "<Start>--" ++ showE (s ++ "|        ") x ++ newLine s ++
                            "<End>--" ++ showE (s ++ "|      ") y ++ newLine s ++
                            "<Step>--" ++ showE (s ++ "|       ") z ++ newLine s ++
                            "<Body>--" ++ showS (s ++ "        ") b   
showS s (Try t e c)       = "[Try]--<Body>--" ++ showS (s ++ "|              ") t ++ newLine s ++
                            "<Value>--" ++ showE (s ++ "|        ") e ++ newLine s ++
                            "<Catch>--" ++ showS (s ++ "         ") c 
showS s (Switch e c d)    = "[Switch]--<Expr>--" ++ showE (s ++ "|                 ") e ++ newLine s ++
                            cases c ++ "<Default>--" ++ showS (s ++ "           ") d where

    cases ((e,c):cs) = "<Case>--<Const>--" ++ showE (s ++ "|                ") e ++ newLine s ++
                       "<Statement>--" ++ showS (s ++ "|            ") c ++ newLine s ++ cases cs
    cases _          = ""



----------------------------------------------------------------------------------------
-- TYPES FOR PARSER --------------------------------------------------------------------
----------------------------------------------------------------------------------------

type ErrorMessage = Maybe String

type Error = (ErrorMessage,Int,Int)
type Str   = (String,Int,Int)

newtype Parser a = P (Str -> Either (a, Str) Error)

instance Monad Parser where
  return a = P (Left . (,) a)
  (P p) >>= f = P $ next f . p where  
    next f (Left (a,s)) = let (P p) = f a in p s
    next _ (Right err)  = Right err 