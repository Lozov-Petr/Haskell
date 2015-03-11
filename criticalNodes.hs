type Node  = Int
type Arc   = (Node, Node)
type Graph = [Arc]
type Path  = [Node]

criticalNodes :: Graph -> Node -> Node -> [Node]
criticalNodes graph a b = case allPaths graph a b of
                          x:xs -> foldl (//) (init $ tail x) xs  
                          []   -> []
                          where

    (//) :: [Node] -> [Node] -> [Node]
    a // b = filter (\x -> elem x b) a 

    allPaths :: Graph -> Node -> Node -> [Path]
    allPaths graph a b = genPaths [[a]] where
        
        genPaths :: [Path] -> [Path]
        genPaths [] = []
        genPaths p  = good ++ genPaths bad where
            (good,bad) = separationPaths $ filter notCicle $ newPaths p
        
        newPaths :: [Path] -> [Path]
        newPaths = concat . map (\path@(x:_) -> map (:path) $ allNeighbors graph x) where

            allNeighbors :: Graph -> Node -> [Node]
            allNeighbors graph n = foldl choiceNeighbor [] graph where
                
                choiceNeighbor :: [Node] -> Arc -> [Node]
                choiceNeighbor acc (x,y) | x == n = y:acc
                                         | y == n = x:acc
                                         | True   =   acc

        separationPaths :: [Path] -> ([Path], [Path])
        separationPaths = foldl goodOrBad ([],[]) where
            
            goodOrBad :: ([Path], [Path]) -> Path -> ([Path], [Path])
            goodOrBad (good,bad) p@(x:_) | x == b = (p:good,bad)
                                         | True   = (good,p:bad)

        notCicle :: Path -> Bool
        notCicle (x:xs) = notElem x xs

graph1 :: Graph
graph1 = [(2,3),(1,2),(3,1),(4,3),(4,6),(5,6),(5,3)]

graph2 :: Graph
graph2 = [(0,1),(0,2),(0,3),(1,4),
          (2,4),(3,4),(4,5),(4,6),
          (5,7),(6,7),(7,8),(3,7),
          (8,9),(8,10),(10,11),(9,11)]