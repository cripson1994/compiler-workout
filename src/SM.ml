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
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let rec eval env ((cs, stack, ((st, i, o, r) as c)) as cf) pl =
	  match pl with
	  | [] -> cf
	  | p :: ps ->
      let eval_d x = eval env x ps
      in match p with
        | BINOP op -> let (y :: x :: stack) = stack
          in eval_d (cs, (Expr.to_func op x y) :: stack, c)
    	  | CONST z  -> eval_d (cs, z :: stack, c)
    	  | WRITE    -> let (z :: stack)      = stack
          in eval_d (cs, stack, (st, i, o @ [z], r))
    	  | READ     -> let (z :: i)          = i
          in eval_d (cs, z :: stack, (st, i, o, r))
    	  | ST x     -> let (z :: stack)      = stack
          in eval_d (cs, stack, ((State.update x z st), i, o, r))
    	  | LD x     -> eval_d (cs, (State.eval st x) :: stack, (st, i, o, r))

	  | LABEL _  -> eval env cf ps
	  | JMP l    -> eval env cf (env#labeled l)
	  | CJMP(s, l) ->
	    let (z::stack) = stack
      in let resolve = function
        | "z"  -> z == 0
	      | "nz" -> z != 0
        in eval env (cs, stack, c) (if resolve s then (env#labeled l) else ps)
	  | BEGIN (xs, locs)  ->
      let rec cut k xs = if k <= 0 then [], xs else
	      match xs with
	        | [] -> failwith "nothing to take"
	        | x::xs -> let fs, ts = cut (k - 1) xs in x :: fs, ts
	    in let args, stack  = cut (List.length xs) stack
      in let st           = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args)
      in eval_d (cs, stack, (st, i, o, r))
	  | END ->  (match cs with
      | (p', st') :: cs' -> let st'' = State.leave st st'
        in eval env (cs', stack, (st'', i, o, r)) p'
	    | [] -> cf )
	  | CALL f ->  let conf = (ps, st) :: cs, stack, c
      in eval env conf (env#labeled f)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o, _)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [], None)) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labelGenerator =
	 object (self)
	    val counter = ref 0
	    method nextLabel =
	        let current = !counter in
	            incr counter; Printf.sprintf "L%d" current
	 end

	 let generator = new labelGenerator

	 let rec compile (functions, prog) =
	   let rec compileExpr = function
	    | Expr.Var   x          -> [ LD x ]
	    | Expr.Const n          -> [ CONST n ]
	    | Expr.Binop (op, x, y) -> compileExpr x @ compileExpr y @ [ BINOP op ]
	    | Expr.Call (name, args) ->
	      List.concat (List.map compileExpr (List.rev args))
	      @ [CALL name]
	   in
	   let rec compile' expr = match expr with
	   | Stmt.Seq (s1, s2)  -> compile' s1 @ compile' s2
	   | Stmt.Read x        -> [ READ; ST x ]
	   | Stmt.Write e       -> compileExpr e @ [ WRITE ]
	   | Stmt.Assign (x, e) -> compileExpr e @ [ ST x ]
	   | Stmt.Skip          -> []
	   | Stmt.If (c, t, e)  ->
	      let elseL = generator#nextLabel in
	      let exitL = generator#nextLabel in
	          compileExpr c
	          @ [CJMP ("z", elseL) ]
	          @ compile' t
	          @ [JMP exitL; LABEL elseL]
	          @ compile' e
	          @ [LABEL exitL]
	    | Stmt.While (c, b) ->
	      let startL = generator#nextLabel in
	      let exitL = generator#nextLabel in
	          [LABEL startL]
	          @ compileExpr c
	          @ [CJMP ("z", exitL)]
	          @ compile' b
	          @ [JMP startL; LABEL exitL]
	    | Stmt.Repeat (b, c) ->
	      let startL = generator#nextLabel in
	          [LABEL startL]
	          @ compile' b
	          @ compileExpr c
	          @ [CJMP ("z", startL)]
	    | Stmt.Call (name, args) ->
	        (List.concat (List.map compileExpr args))
	          @ [CALL name]
	    | Stmt.Return (Some expr) ->
	        compileExpr expr
	          @ [END]
	    | Stmt.Return None -> [END]
	  in
	  let compileFunction (name, (p, l, b)) =
	    [LABEL name; BEGIN (p, l)]
	    @ compile' b
	    @ [END]
	  in
	  let mainLabel = generator#nextLabel in
	  [JMP mainLabel]
	  @ List.concat (List.map compileFunction functions)
	  @ [LABEL mainLabel]
	  @ (compile' prog) 
