open GT       
open Language
open Language.Expr
open Language.Stmt
       
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
let rec eval cfg prg = match prg with
        | [] -> cfg
        | p :: ps -> match p with
            | BINOP binop -> (match cfg with
                | (x :: y :: xs, (st, inp, out)) ->  eval (((Language.Expr.eval st (Binop (binop, Const y, Const x)))) :: xs, (st, inp, out)) ps
                | _ -> failwith "eval BINOP failed!"
			)
			| CONST c -> (match cfg with
                | (xs, stcfg) ->  eval (c :: xs, stcfg) ps
                | _ -> failwith "eval CONST failed!"
			)
			| READ -> (match cfg with
                | (xs, (st, i :: inp, out)) ->  eval (i :: xs, (st, inp, out)) ps
                | _ -> failwith "eval READ failed!"
			)
			| WRITE -> (match cfg with
                | (x :: xs, (st, inp, out)) ->  eval (xs, (st, inp, out @ [x])) ps
                | _ -> failwith "eval WRITE failed!"
			)
			| LD var -> (match cfg with
                | (xs, (st, inp, out)) ->  eval ((st var) :: xs, (st, inp, out)) ps
                | _ -> failwith "eval LD failed!"
			)
			| ST var -> (match cfg with
                | (x :: xs, (st, inp, out)) ->  eval (xs, ((Language.Expr.update var x st), inp, out)) ps
                | _ -> failwith "eval LD failed!"
			)
       | _ -> failwith "eval unknown instruction!"

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
