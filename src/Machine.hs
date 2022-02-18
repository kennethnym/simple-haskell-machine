{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Prelude
import Data.Map

type Vname = [Char]

type Val = Int

type State = Map Vname Val

data Instr =
        LOADI Val   |
        LOAD Vname  |
        ADD         |
        STORE Vname |
        JMP Int     |
        JMPLESS Int |
        JMPGE Int
        deriving (Eq, Read, Show)

type Stack = [Int]

type Config = (Int, State, Stack)

-- inc increases the given Int by one
inc :: Integral a => a -> a
inc = (+ 1)

iexec :: Instr -> Config -> Config
iexec (LOADI val) (counter, state, stack) = (inc counter, state, val:stack)

iexec (LOAD var) (counter, state, stack) = (inc counter, state, (state ! var):stack)

-- Stack has more than two integers, add them and append to top of stack
iexec ADD (counter, state, a:b:xs) = (inc counter, state, (a + b):xs)

-- Stack has values, pop the stack and insert the variable to the state
iexec (STORE var) (counter, state, x:xs) = (inc counter, newState, xs)
    where newState = insert var x state

iexec (JMP i) (counter, state, stack) = (inc counter + i, state, stack)

iexec (JMPLESS i) (counter, state, x:y:xs) = (if x > y then newCounter + i else newCounter, state, xs)
    where newCounter = inc counter

iexec (JMPGE i) (counter, state, x:y:xs)   = (if y >= x then newCounter + i else newCounter, state, xs)
    where newCounter = inc counter

exec :: [Instr] -> Config -> Config
exec [] config = config
exec instructions (counter, state, stack) =
    if counter >= length instructions
    then (counter, state, stack)
    else exec instructions newConfig
        where newConfig = iexec (instructions !! counter) (counter, state, stack)
