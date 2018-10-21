open GT
open Language

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string
(* conditional jump                *) | CJMP  of string * string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env (st, (s, i, o)) stm =
    match (st, (s, i, o)), stm with
    | conf, [] -> conf
    | (y::x::st, sio), (BINOP op)::stm -> eval env ((Language.Expr.evalBinop op x y)::st, sio) stm
    | (st, sio), (CONST z)::stm -> eval env (z::st, sio) stm
    | (st, (s, z::i, o)), (READ)::stm -> eval env (z::st, (s, i, o)) stm
    | (z::st, (s, i, o)), (WRITE)::stm -> eval env (st, (s, i, o @ [z])) stm
    | (st, (s, i, o)), (LD x)::stm -> eval env ((s x)::st, (s, i, o)) stm
    | (z::st, (s, i, o)), (ST x)::stm -> eval env (st, ((Language.Expr.update x z s), i, o)) stm
    | conf, (LABEL l)::stm -> eval env conf stm
    | conf, (JMP l)::stm -> eval env conf (env#labeled l)
    | (z::st, sio), (CJMP (cond, l))::stm -> if ((cond = "z" && z = 0) || (cond = "nz" && z <> 0))
      then eval env (st, sio) (env#labeled l)
      else eval env (st, sio) stm
(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)

class labelGenerator =
  object (self)
    val counter = ref 0
    method nextLabel = let current = !counter in
      incr counter;
      Printf.sprintf "L%d" current
  end

let generator = new labelGenerator

let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)   -> compile s1 @ compile s2
  | Stmt.Read x         -> [READ; ST x]
  | Stmt.Write e        -> expr e @ [WRITE]
  | Stmt.Assign (x, e)  -> expr e @ [ST x]
  | Stmt.Skip           -> []
  | Stmt.If (e, s1, s2) ->
    let elseLabel = generator#nextLabel in
    let endLabel = generator#nextLabel in
    expr e @ [CJMP ("z", elseLabel)] @ compile s1 @ [JMP endLabel; LABEL elseLabel] @ compile s2 @ [LABEL endLabel]
  | Stmt.While (e, s)   ->
    let loopLabel = generator#nextLabel in
    let exitLabel = generator#nextLabel in
    [LABEL loopLabel] @ expr e @ [CJMP ("z", exitLabel)] @ compile s @ [JMP loopLabel; LABEL exitLabel]
  | Stmt.Repeat (e, s)  ->
    let loopLabel = generator#nextLabel in
    [LABEL loopLabel] @ compile s @ expr e @ [CJMP ("z", loopLabel)]
