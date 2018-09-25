open GT
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let rec eval (st, (s, i, o)) stm =
  match (st, (s, i, o)), stm with
  | conf, [] -> conf
  | (y::x::st, sio), (BINOP op)::stm -> eval ((Language.Expr.evalBinop op x y)::st, sio) stm
  | (st, sio), (CONST z)::stm -> eval (z::st, sio) stm
  | (st, (s, z::i, o)), (READ)::stm -> eval (z::st, (s, i, o)) stm
  | (z::st, (s, i, o)), (WRITE)::stm -> eval (st, (s, i, o @ [z])) stm
  | (st, (s, i, o)), (LD x)::stm -> eval ((s x)::st, (s, i, o)) stm
  | (z::st, (s, i, o)), (ST x)::stm -> eval (st, ((Language.Expr.update x z s), i, o)) stm

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compileExpr expr =
  match expr with
  | Language.Expr.Const n -> [CONST n]
  | Language.Expr.Var x -> [LD x]
  | Language.Expr.Binop (op, x, y) -> (compileExpr x) @ (compileExpr y) @ [BINOP op]

let rec compile stmt =
  match stmt with
  | Language.Stmt.Assign (x, e) -> (compileExpr e) @ [ST x]
  | Language.Stmt.Read x -> [READ; ST x]
  | Language.Stmt.Write e -> (compileExpr e) @ [WRITE]
  | Language.Stmt.Seq (s1, s2) -> (compile s1) @ (compile s2)
