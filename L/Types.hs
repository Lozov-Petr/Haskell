module Types where

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

       | Not E   | Inv E
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
       | Break    L
       | Continue L
       | Write    E
       | Read     Elem
       | Assign   Elem E
       | Sq       S S
       | IfTE     L E S S
       | While    L E S
       | Try      L S E S
       | Throw    E

data Elem = EV V
          | EA Elem E
          | ES Elem V

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

instance Show Elem where
      show = showElem ""

instance Show E where 
  show = showE ""

instance Show S where
      show = showS ""

instance Show P where
      show (Program p) = "Tree:\n\n    " ++ showS "    " p ++ "\n" 


---------------------------
showE  :: String -> E -> String
---------------------------
showE _ (Num a) = "(N)--" ++ show a
showE _ (Var s) = "(V)--" ++ s
showE _ (EOF)   = "(EOF)"

showE s (Not e) = "(!)--" ++ showE (s ++ "     ") e
showE s (Inv e) = "(~)--" ++ showE (s ++ "     ") e

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

showE s (CreateA e)       = "(CreateArray)--" ++ showE (s ++ "               ") e


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
showS s (Throw e)        = "[Throw]--" ++ showE (s ++ "         ") e
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
showS s (Try l t e c)    = "[Try]--<Label>--" ++ l ++ "\n" ++ s ++ "|\n" ++ s ++
                           "<Body>--" ++ showS (s ++ "|       ") t ++ "\n" ++ s ++ "|\n" ++ s ++
                           "<Value>--" ++ showE (s ++ "|        ") e ++ "\n" ++ s ++ "|\n" ++ s ++
                           "<Catch>--" ++ showS (s ++ "         ") c 


showElem :: String -> Elem -> String
showElem _ (EV v)    = "{V}--" ++ v
showElem s (ES ea v) = "{.}--" ++ showElem (s ++ "|    ") ea ++ "\n" ++ s ++ "|\n" ++ s ++ "{V}--" ++ v
showElem s (EA ea e) = "{[]}--" ++ showElem (s ++ "|     ") ea ++ "\n" ++ s ++ "|\n" ++ s ++ showE s e