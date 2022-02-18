module Main where

import System.Environment
import Compiler
import Machine (Instr)
import Interpreter (Com)

main :: IO ()
main = do
    input <- getArgs
    let cmd = read $ head input :: Com
    print (ccomp cmd)
