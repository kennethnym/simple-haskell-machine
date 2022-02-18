module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

acomp :: AExp -> [Instr]
acomp (N int)          = [LOADI int]
acomp (V var)          = [LOAD var]
acomp (Plus exp1 exp2) = acomp exp1 ++ acomp exp2 ++ [ADD]

bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc bool) match jmp = [JMP jmp | bool == match]

bcomp (Not expr) match jmp = bcomp expr (not match) jmp

bcomp (Less exp1 exp2) match jmp =
    acomp exp1 ++ acomp exp2 ++
    if match
    then [JMPLESS jmp]
    else [JMPGE jmp]

bcomp (And exp1 exp2) False jmp =
    bcomp exp1 False (length exp2Instr + jmp) ++ exp2Instr
    where exp2Instr = bcomp exp2 False jmp

bcomp (And exp1 exp2) True jmp =
    bcomp exp1 False (length exp2Instr) ++ exp2Instr
    where exp2Instr = bcomp exp2 True jmp

ccomp :: Com -> [Instr]
ccomp SKIP = []

ccomp (Assign var val) = acomp val ++ [STORE var]

ccomp (While cond cmd) =
    whileBlock ++ [JMP (-(length whileBlock) - 1)]
    where
        whileBlock = bcomp cond False (length instr + 1) ++ instr
        instr      = ccomp cmd

ccomp (If cond thenCode elseCode) =
    bcomp cond False (length thenInstr + 1) ++ thenInstr ++ [JMP (length elseInstr)] ++ elseInstr
    where
        thenInstr = ccomp thenCode
        elseInstr = ccomp elseCode

ccomp (Seq cmd1 cmd2) = ccomp cmd1 ++ ccomp cmd2
