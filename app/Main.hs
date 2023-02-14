module Main (main) where

import Lib

{--
main :: IO ()
main = someFunc
--}

main :: IO ()
main = do
    putStrLn "What is your first num?"
    x <- getLine >>= (return . read)
    y <- getLine >>= (return . read)
    putStrLn $ "sum of nums is: " ++ show (x + y)
