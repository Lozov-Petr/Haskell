module Main where

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
          
          let tree = parser (code ++ "\n")
          print tree

          printSeparate

          parameters <- readParameters

          printSeparate

          putStrLn . resultToString $ interpretP tree parameters

          printSeparate



---------------------------
printSeparate :: IO ()
---------------------------
printSeparate = putStrLn $ map (const '-') [1..30]


---------------------------
shiftCode :: String -> String
---------------------------
shiftCode = unlines . map ("    " ++) . lines


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