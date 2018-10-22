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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env conf stm =
    match conf, stm with
    | conf, [] -> conf
    | (cs, y::x::st, (s, i, o)), (BINOP op)::stm -> eval env (cs, (Expr.eval s (Expr.Binop (op, Expr.Const x, Expr.Const y)))::st, (s, i, o)) stm
    | (cs, st, sio), (CONST z)::stm -> eval env (cs, z::st, sio) stm
    | (cs, st, (s, z::i, o)), (READ)::stm -> eval env (cs, z::st, (s, i, o)) stm
    | (cs ,z::st, (s, i, o)), (WRITE)::stm -> eval env (cs, st, (s, i, o @ [z])) stm
    | (cs, st, (s, i, o)), (LD x)::stm -> eval env (cs, (State.eval s x)::st, (s, i, o)) stm
    | (cs, z::st, (s, i, o)), (ST x)::stm -> eval env (cs, st, ((Language.State.update x z s), i, o)) stm
    | conf, (LABEL l)::stm -> eval env conf stm
    | conf, (JMP l)::stm -> eval env conf (env#labeled l)
    | (cs, z::st, sio), (CJMP (cond, l))::stm -> if ((cond = "z" && z = 0) || (cond = "nz" && z <> 0))
      then eval env (cs, st, sio) (env#labeled l)
      else eval env (cs, st, sio) stm
    | (cs, st, (s, i, o)), (BEGIN (params, locals))::stm ->
      let scope = State.push_scope s (params @ locals) in
      let (s', st') = List.fold_left (fun (s, z::st) x -> (State.update x z s, st)) (scope, st) params in
      eval env (cs, st', (s', i, o)) stm
    | (cs, st, (s, i, o)), (CALL f)::stm -> eval env ((stm, s)::cs, st, (s, i, o)) (env#labeled f)
    | conf, END::[] -> conf
    | ((p', s')::cs, st, (s, i, o)), END::stm -> eval env (cs, st, (Language.State.drop_scope s s', i, o)) p'


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
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

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

 let rec compile (funcs, prog) =
   let rec compile_expr = function
   | Expr.Var   x          -> [LD x]
   | Expr.Const n          -> [CONST n]
   | Expr.Binop (op, x, y) -> compile_expr x @ compile_expr y @ [BINOP op]
   in let rec compile' expr = match expr with
     | Stmt.Seq (s1, s2)   -> compile' s1 @ compile' s2
     | Stmt.Read x         -> [READ; ST x]
     | Stmt.Write e        -> compile_expr e @ [WRITE]
     | Stmt.Assign (x, e)  -> compile_expr e @ [ST x]
     | Stmt.Skip           -> []
     | Stmt.If (e, s1, s2) ->
       let elseLabel = generator#nextLabel in
       let endLabel = generator#nextLabel in
       compile_expr e @ [CJMP ("z", elseLabel)] @ compile' s1 @ [JMP endLabel; LABEL elseLabel] @ compile' s2 @ [LABEL endLabel]
     | Stmt.While (e, s)   ->
       let loopLabel = generator#nextLabel in
       let exitLabel = generator#nextLabel in
       [LABEL loopLabel] @ compile_expr e @ [CJMP ("z", exitLabel)] @ compile' s @ [JMP loopLabel; LABEL exitLabel]
     | Stmt.Repeat (s, e)  ->
       let loopLabel = generator#nextLabel in
       [LABEL loopLabel] @ compile' s @ compile_expr e @ [CJMP ("z", loopLabel)]
     | Stmt.Call (f, a) -> (List.concat (List.map compile_expr a)) @ [CALL f]
    in let compile_function (name, (params, locals, body)) = [LABEL name; BEGIN (params, locals)] @ compile' body @ [END]
    in let mainLabel = generator#nextLabel
    in [JMP mainLabel] @ List.concat (List.map compile_function funcs) @ [LABEL mainLabel] @ compile' prog
