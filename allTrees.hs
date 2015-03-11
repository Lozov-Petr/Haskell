-- Дерево ------------------------------------------------------------------------------------------------
data Tree = C Integer
          | V String  
          | ADD Tree Tree 
          | AND Tree Tree 
          | EQL Tree Tree 
          | DIV Tree Tree deriving (Eq)

-- Вывод -------------------------------------------------------------------------------------------------
instance Show Tree where
  show tree = "\n   " ++ printTree "   " tree ++ "\n" where
    printTree _   (V s)     = s
    printTree _   (C b)     = show b
    printTree str (ADD l r) = printNode str "+" l r
    printTree str (AND l r) = printNode str "&" l r
    printTree str (EQL l r) = printNode str "=" l r
    printTree str (DIV l r) = printNode str "/" l r
       
    printNode str op l r = op ++ "--" ++ printTree (str ++ "|  ") r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l

-- Список всех деревьев ----------------------------------------------------------------------------------
allTrees = generate varNames constValues [] [] where
  
  varNames = addToBegin tails varHeadChars where
    varHeadChars = ['a'..'z'] ++ ['A'..'Z']
    varTailChars = '_' : ['0'..'9'] ++ varHeadChars
    addToBegin strs chars = [c:s | s <- strs, c <- chars]
    tails = "" : addToBegin tails varTailChars

  constValues = [0..]

  generate (v:vs) (c:cs) visTrees curTrees = unvisTrees ++ generate vs cs (visTrees ++ unvisTrees) newTrees where
    unvisTrees = V v : C c : curTrees

    newTrees =  create   visTrees unvisTrees 
             ++ create unvisTrees   visTrees
             ++ create unvisTrees unvisTrees
    
    create tsl tsr = [node tl tr | node <- nodes, tl <- tsl, tr <- tsr] 
    
    nodes = [ADD, AND, EQL, DIV]

-- Проверка ----------------------------------------------------------------------------------------------
withoutRepeats n = all (\t -> (1==) . length . filter (==t) $ trees) trees where trees = take n allTrees

exist = flip elem $ allTrees
