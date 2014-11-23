module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn "What's your name?"
    args <- getLine
    putStrLn ("Hello, " ++ args)
