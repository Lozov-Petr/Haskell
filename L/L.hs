module L where

import Types (P, Error)
import Interpret (interpretP)
import Parser (parser)
import System.IO
import System.Environment (getArgs)
import Control.Monad (when)

help = "\n    help: L [path] [ -c | -t | -ni ]\n"

---------------------------
main :: IO ()
---------------------------
main = do hSetBuffering stdout NoBuffering

          args <- getArgs

          if null args then putStrLn help else realMain args

                  
---------------------------
realMain :: [String] -> IO ()
---------------------------
realMain (path:params) = do let mustShowCode  =       elem "-c"  params
                                mustShowTree  =       elem "-t"  params
                                mustInterpret = not $ elem "-ni" params
                                
                            code <- readFile path

                            when mustShowCode (printSeparate >> putStrLn "Code:" >> putStrLn (shiftCode code))
                                    
                            treeToResult mustShowTree mustInterpret $ parser (code ++ "\n")

                            when (mustShowTree || mustShowCode || mustInterpret) printSeparate


---------------------------
treeToResult :: Bool -> Bool -> Either P Error -> IO ()
---------------------------
treeToResult mst mi (Left p) = do when mst $ printSeparate >> print p
                                  when mi  $ runInterpret p
treeToResult _ _ (Right (Just err,i,j)) = do putStr (show (i,j) ++ " Parsing error: " ++ err)
treeToResult _ _ (Right (Nothing, i,j)) = do putStr (show (i,j) ++ " Parsing error: Unknown error")


---------------------------
printSeparate :: IO ()
---------------------------
printSeparate = putStrLn $ map (const '-') [1..100]


---------------------------
runInterpret :: P -> IO ()
---------------------------
runInterpret p = do printSeparate
                    parameters <- readParameters
                    printSeparate
                    putStrLn . resultToString $ interpretP p parameters

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