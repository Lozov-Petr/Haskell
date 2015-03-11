import System.IO

data TapeBF = T [Int]  Int  [Int]  deriving Show
data CodeBF = C [Char] Char [Char] deriving Show

-----------------------
empty :: TapeBF
-----------------------
empty = T [] 0 []

-----------------------
stringToCodeBF :: String -> CodeBF
-----------------------
stringToCodeBF []     = C [] '$'   []
stringToCodeBF (x:xs) = C []  x  $ xs ++ "$"

class Tape t where
  incr   :: t -> t
  decr   :: t -> t
  shiftl :: t -> t
  shiftr :: t -> t
  get    :: t -> Int
  set    :: t -> Int -> t
  
class Code c where
  curr :: c -> Char
  next :: c -> c
  prev :: c -> c
  
  -----------------------
  toClosingBracket :: c -> c
  -----------------------
  toClosingBracket code = find (next code) 1 where
    find code acc = case (curr code, acc) of
                      (']', 1) -> code
                      (']', _) -> findNC $ acc - 1
                      ('[', _) -> findNC $ acc + 1
                      ( _ , _) -> findNC   acc
                    where findNC = find $ next code
                   
  -----------------------                  
  toOpeningBracket :: c -> c
  -----------------------
  toOpeningBracket code = find (prev code) 1 where
    find code acc = case (curr code, acc) of
                      ('[', 1) -> code
                      ('[', _) -> findNC $ acc - 1
                      (']', _) -> findNC $ acc + 1
                      ( _ , _) -> findNC   acc
                    where findNC = find $ prev code
                    
instance Tape TapeBF where
  incr (T l 255 r) = T l  0      r
  incr (T l c   r) = T l (c + 1) r
  
  decr (T l 0   r) = T l  255    r
  decr (T l c   r) = T l (c - 1) r
  
  shiftl (T []     c r) = T [] 0 (c:r)
  shiftl (T (l:ls) c r) = T ls l (c:r)
  
  shiftr (T l c []    ) = T (c:l) 0 []
  shiftr (T l c (r:rs)) = T (c:l) r rs
  
  get (T _ c _) = c
  set (T l _ r) x 
    | x >= 0 && x <= 255 = T l x r
    | True               = error "Incorrect number!"
    
instance Code CodeBF where
  curr (C _ c _) = c
  
  next (C _ _ []    ) = error "Where is ']'?!"
  next (C l c (r:rs)) = C (c:l) r rs
  
  prev (C []     _ _) = error "Where is '['?!"
  prev (C (l:ls) c r) = C ls l (c:r)
 
-----------------------  
runBF :: IO ()
-----------------------
runBF = step getCodeBF (return empty) where
  
  -----------------------
  getCodeBF :: IO CodeBF
  -----------------------
  getCodeBF  = do
               putStr "Input path to code: "
               path   <- getLine
               putStr "\nStart\n"
               handle <- openFile path ReadMode
               code   <- hGetContents handle
               return $ stringToCodeBF code 

  -----------------------
  step :: IO CodeBF -> IO TapeBF -> IO ()
  -----------------------
  step codeIO tapeIO = do 
                       tape <- tapeIO
                       code <- codeIO
                       let stepNC  = step $ return $ next code
                       let stepNCR = stepNC . return
                       let currSym = curr code

                       case currSym of
                         '+' -> stepNCR $ incr   tape
                         '-' -> stepNCR $ decr   tape
                         '>' -> stepNCR $ shiftr tape
                         '<' -> stepNCR $ shiftl tape
                         '[' -> case get tape of
                                0 -> step (return $ next $ toClosingBracket code) tapeIO
                                _ -> stepNC tapeIO
                         ']' -> case get tape of
                                0 -> stepNC tapeIO
                                _ -> step (return $ next $ toOpeningBracket code) tapeIO
                         '.' -> do 
                                putStrLn . (++) "Output: " . show . get $ tape
                                stepNC tapeIO
                         ',' -> do 
                                putStr "Input : "
                                x <- getLine 
                                stepNCR $ set tape $ read x
                         '$' -> putStr "Done"
                         _   -> stepNC tapeIO  


-----------------------
main :: IO ()
-----------------------
main = runBF                                               