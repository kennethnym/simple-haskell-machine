module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

data AExp =
    N Int          |
    V Vname        |
    Plus AExp AExp
    deriving (Eq, Read, Show)

aval :: AExp -> State -> Val
aval (N x) _                = x
aval (V var) state          = state ! var
aval (Plus exp1 exp2) state = aval exp1 state + aval exp2 state

data BExp =
    Bc  Bool       |
    Not BExp       |
    And BExp BExp  |
    Less AExp AExp
    deriving (Eq, Read, Show)

bval :: BExp -> State -> Bool
bval (Bc bool) _             = bool
bval (Not bool) state        = not (bval bool state)
bval (And bool1 bool2) state = bval bool1 state && bval bool2 state
bval (Less exp1 exp2) state  = aval exp1 state < aval exp2 state

data Com =
    Assign Vname AExp    |
    Seq Com Com          |
    If BExp Com Com      |
    While BExp Com       |
    SKIP
    deriving (Eq, Read, Show)

eval :: Com -> State -> State
eval (Assign var expr) state = insert var value state
    where value = aval expr state

eval (Seq com1 com2) state = (eval com2 . eval com1) state

eval (If cond thenCom elseCom) state =
    if bval cond state
    then eval thenCom state
    else eval elseCom state

eval (While cond com) state =
    if bval cond state
    then eval (While cond com) (eval com state)
    else state

eval SKIP state = state
