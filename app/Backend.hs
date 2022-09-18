{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}

module Backend where
import Syntax

data Atom
    = ALit Literal
    | AVar Name
    | ALabel Name
data MExpr
    = MLet Prim [Atom]
    | MRet Atom


data Reg
    = Rax | Rcx | Rdx | Rbx
    | Rsp | Rbp | Rsi | Rdi
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15
    | Rip

data Oprand
    = Const Literal
    | InMem Reg Int
    | InReg Reg

data Opcode
    = Addq Oprand Oprand
    | Subq Oprand Oprand
    | Negq Oprand
    | Movq Oprand Oprand
    | Pushq Oprand
    | Popq Oprand
    | Callq  Oprand
    | Retq

type Block = [Opcode]


