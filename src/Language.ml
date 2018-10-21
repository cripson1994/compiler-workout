(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT
open Ostap
(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let castToInt b =
          match b with
          | true -> 1
          | _ -> 0

       let castToBool i =
          match i with
          | 0 -> false
          | _ -> true

       let evalBinop op val1 val2 =
          match op with
          | "+" -> val1 + val2
          | "-" -> val1 - val2
          | "*" -> val1 * val2
          | "/" -> val1 / val2
          | "%" -> val1 mod val2
          | "<" -> castToInt (val1 < val2)
          | "<=" -> castToInt (val1 <= val2)
          | ">" -> castToInt (val1 > val2)
          | ">=" -> castToInt (val1 >= val2)
          | "==" -> castToInt (val1 == val2)
          | "!=" -> castToInt (val1 <> val2)
          | "&&" -> castToInt ((castToBool val1) && (castToBool val2))
          | "!!" -> castToInt ((castToBool val1) || (castToBool val2))

       let rec eval ctx exp =
          match exp with
          | Const c -> c
          | Var v -> ctx v
          | Binop (op, exp1, exp2) -> evalBinop op (eval ctx exp1) (eval ctx exp2)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string

    *)
    let toBinop op = fun x y -> Binop (op, x, y)

    ostap (
       expr: !(Ostap.Util.expr (fun x -> x)
           [|
             `Lefta , [ostap ("!!"), toBinop "!!"];
             `Lefta , [ostap ("&&"), toBinop "&&"];
             `Nona  , [ostap ("<="), toBinop "<=";
                       ostap (">="), toBinop ">=";
                       ostap ("<"),  toBinop "<" ;
                       ostap (">"),  toBinop ">"];
             `Nona  , [ostap ("=="), toBinop "==";
                       ostap ("!="), toBinop "!="];
             `Lefta , [ostap ("+"),  toBinop "+";
                       ostap ("-"),  toBinop "-"];
             `Lefta , [ostap ("*"),  toBinop "*";
                       ostap ("/"),  toBinop "/";
                       ostap ("%"),  toBinop "%"];
           |]
           primary
         );
         primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" expr -")"
     )

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (s, i, o) t =
      match (s, i, o), t  with
        | (s, z::i, o), Read x -> ((Expr.update x z s), i, o)
        | (s, i, o), Write e -> (s, i, o @ [Expr.eval s e])
        | (s, i, o), Assign (x, e) -> ((Expr.update x (Expr.eval s e) s), i, o)
        | conf, Seq (t1, t2)  -> eval (eval conf t1) t2
        | conf, Skip -> conf
        | (s, i, o), If (expr, s1, s2) -> if ((Expr.eval s expr) != 0)
          then eval (s, i, o) s1
          else eval (s, i, o) s2
        | conf, While (expr, st) -> if ((Expr.eval s expr) != 0)
          then eval conf (Seq (st, While (expr, st)))
          else eval conf Skip
        | conf, Repeat (expr, st) -> let (s', i', o') = eval conf st
          in if ((Expr.eval s' expr) != 0)
            then eval (s', i', o') (Repeat (expr, st))
            else eval (s', i', o') Skip

    (* Statement parser *)
    ostap (
      simple_stmt:
          x:IDENT ":=" e:!(Expr.expr) {Assign (x, e)} (* Assign *)
        | "read" -"(" x:IDENT -")" {Read x} (* Read *)
        | "write" -"(" e:!(Expr.expr) -")" {Write e} (* Write *)
        | "skip" { Skip }; (* Skip *)

        ifBlock:
           "if" e:!(Expr.expr) "then" b:parse el:elseBlock "fi" { If (e, b, el) }
         | "if" e:!(Expr.expr) "then" b:parse "fi" { If (e, b, Skip )} ;

        elseBlock:
          "elif" e:!(Expr.expr) "then" b:parse next:elseBlock { If(e, b, next) }
        | "else" b:parse { b }
        | "elif" e:!(Expr.expr) "then" b:parse { If (e, b, Skip) };

        whileBlock:
          "while" e:!(Expr.expr) "do" b:parse "od" { While (e, b) };

        repeatBlock:
          "repeat" b:parse "until" e:!(Expr.expr) { Repeat (e, b) };

        forBlock:
           "for " init:simple_stmt "," cond:!(Expr.expr) "," upd:simple_stmt "do" b:parse "od" { Seq(init, While(cond, Seq(b, upd))) };

        stmt: simple_stmt | ifBlock | whileBlock | repeatBlock | forBlock;
        parse: <s::ss> :
          !(Util.listBy)
          [ostap (";")]
          [stmt]
          { List.fold_left (fun s ss -> Seq (s, ss)) s ss}
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse
