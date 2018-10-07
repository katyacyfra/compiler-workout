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
let rec eval env cfg prg = match prg with
        | [] -> cfg
        | p :: ps -> match p with
            | BINOP binop -> (match cfg with
                | (x :: y :: xs, (st, inp, out)) ->  eval env (((Language.Expr.eval st (Binop (binop, Const y, Const x)))) :: xs, (st, inp, out)) ps
                | _ -> failwith "eval BINOP failed!"
			)
			| CONST c -> (match cfg with
                | (xs, stcfg) ->  eval env (c :: xs, stcfg) ps
                | _ -> failwith "eval CONST failed!"
			)
			| READ -> (match cfg with
                | (xs, (st, i :: inp, out)) ->  eval env (i :: xs, (st, inp, out)) ps
                | _ -> failwith "eval READ failed!"
			)
			| WRITE -> (match cfg with
                | (x :: xs, (st, inp, out)) ->  eval env (xs, (st, inp, out @ [x])) ps
                | _ -> failwith "eval WRITE failed!"
			)
			| LD var -> (match cfg with
                | (xs, (st, inp, out)) ->  eval env ((st var) :: xs, (st, inp, out)) ps
                | _ -> failwith "eval LD failed!"
			)
			| ST var -> (match cfg with
                | (x :: xs, (st, inp, out)) ->  eval env (xs, ((Language.Expr.update var x st), inp, out)) ps
                | _ -> failwith "eval LD failed!"
			)
			| LABEL l -> eval env cfg ps
			| JMP l   -> eval env cfg (env#labeled l)
			| CJMP(c, l) -> (match cfg with
			    | (xs, (st, inp, out))  -> (match xs with
				    | [] -> cfg
					| y :: ys -> if ((c = "z") = (y = 0)) then eval env cfg (env#labeled l) else eval env cfg ps
				)
				| _ -> failwith "eval CJMP failed!"
			)
       | _ -> failwith "eval unknown instruction!"

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

let rec compile' l =
  let label_int k = Printf.sprintf ("L%d") k in
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> let a, n = compile' l s1 in 
                             let b, m = compile' n s2 in a @ b, m
  | Stmt.Read x        -> [READ; ST x], l
  | Stmt.Write e       -> expr e @ [WRITE], l
  | Stmt.Assign (x, e) -> expr e @ [ST x], l
  | Stmt.Skip          -> [], l
  | Stmt.While (c, st) -> let lp, n = compile' (l + 2) st in
         [LABEL (label_int l)] 
		 @ (expr c) 
		 @ [CJMP ("z", (label_int (l + 1)))] 
		 @ lp @ [JMP (label_int l)] 
		 @ [LABEL (label_int (l + 1))], n
  | Stmt.Until (c, st)   -> let lp, n = compile' (l + 1) st in
         [LABEL (label_int l)] 
		 @ lp 
		 @ (expr c) 
		 @ [CJMP ("z", (label_int l))], n
  | Stmt.If (c, tr, fl) -> let th, n = compile' (l + 1) tr in
                           let el, m = compile' (n + 1) fl in
                           (expr c) 
						   @ [CJMP ("z", (label_int l))] 
						   @ th 
						   @ [JMP (label_int n)] 
						   @ [LABEL (label_int l)] 
						   @ el 
						   @ [JMP (label_int n)] 
						   @ [LABEL (label_int n)], m
let compile prg = let smprg, _ = compile' 0 prg in smprg
