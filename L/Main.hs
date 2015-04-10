module Main where

import ParserBase (Error)
import Types (P)
import Interpret (interpretP)
import Parser (parser)

---------------------------
main :: IO ()
---------------------------
main = do putStr "Enter path of file: "
          path <- getLine
          
          printSeparate

          code <- readFile path
          
          putStrLn "Code:"
          putStrLn $ shiftCode code

          printSeparate
          
          treeToResult $ parser (code ++ "\n")

---------------------------
treeToResult :: Either P Error -> IO ()
---------------------------
treeToResult (Left p) = do print p
                           printSeparate
                           parameters <- readParameters
                           printSeparate
                           putStrLn . resultToString $ interpretP p parameters
                           printSeparate

treeToResult (Right (Just err,i,j)) = do putStr (show (i,j) ++ " Parsing error: " ++ err)
treeToResult (Right (Nothing, i,j)) = do putStr (show (i,j) ++ " Parsing error: Unknown error")
---------------------------
printSeparate :: IO ()
---------------------------
printSeparate = putStrLn $ map (const '-') [1..30]


---------------------------
shiftCode :: String -> String
---------------------------
shiftCode = unlines . map (\(s,n,m) -> "    " ++ zeros n m ++ show n ++ ")  " ++ s) . numberingCode . lines where
     numberingCode l = zip3 l l' [len | _ <- l'] where l' = [1..len]
                                                       len = length l
     
     zeros n m | n /= 0 = zeros (div n 10) (div m 10)
               | m /= 0 = '0' : zeros 0 (div m 10)
               | True   = ""


---------------------------
readParameters :: IO [Integer]
---------------------------
readParameters = do putStr "Enter count of pareneters: "
                    countStr <- getLine
                    let count = read countStr
                    pars <- foldl readParameter (return []) [1..count]
                    return $ reverse pars


---------------------------
readParameter :: IO [Integer] -> Integer -> IO [Integer]
---------------------------
readParameter acc i = do xs <- acc
                         putStr $ "Enter parameter " ++ show i ++ ": "
                         xStr <- getLine
                         let x = read xStr
                         return $ x:xs


---------------------------
resultToString :: Maybe [Integer] -> String
---------------------------
resultToString (Just l) = "Result: " ++ show l
resultToString  _       = "Error"