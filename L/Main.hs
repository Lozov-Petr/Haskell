module Main where

import ParserBase (Error)
import Types (P)
import Interpret (interpretP)
import Parser (parser)
import System.IO

---------------------------
main :: IO ()
---------------------------
main = do hSetBuffering stdout NoBuffering

          putStr "Enter path of file: "
          
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
printSeparate = putStrLn $ map (const '-') [1..100]


---------------------------
shiftCode :: String -> String
---------------------------
shiftCode c = unlines $ zipWith (\n l -> "   " ++ showNumber m n ++ ") " ++ l) [1..n] l where
  l = lines c
  n = length l
  m = length $ show n

---------------------------
showNumber :: Int -> Int -> String
---------------------------
showNumber m n = let s = show n in ['0' | _ <- [1..m - length s]] ++ s


---------------------------
readParameters :: IO [Integer]
---------------------------
readParameters = do putStr "Enter count of parameters: "
                    countStr <- getLine
                    let count = read countStr
                    let m = length $ show count
                    pars <- foldl (readParameter m) (return []) [1..count]
                    return $ reverse pars
                 where

  ---------------------------
  readParameter :: Int -> IO [Integer] -> Int -> IO [Integer]
  ---------------------------
  readParameter m acc i = do xs <- acc
                             putStr $ "Enter parameter " ++ showNumber m i ++ ": "
                             xStr <- getLine
                             let x = read xStr
                             return $ x:xs


---------------------------
resultToString :: Maybe [Integer] -> String
---------------------------
resultToString (Just l) = "Result: " ++ show l
resultToString  _       = "Error"