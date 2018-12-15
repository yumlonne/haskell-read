module Main where
import InterPreter
import InterPreter.Env

main :: IO ()
main = do
    file <- readFile "program.txt"
    let ops = map read (lines file)
    execAll ops InterPreter.Env.init
    return ()
