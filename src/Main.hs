module Main where

import System.Environment (getArgs)
import Eval (eval)
import Syntax (filep, fparse)

main :: IO ()
main = do
    args <- getArgs
    txts <- sequence $ map readFile args
    res <- fparse filep $ concat txts
    case eval res of
        Left e -> fail e
        Right r -> putStr r
