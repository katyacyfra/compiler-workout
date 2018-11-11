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


let rec addArgs args cfg = match args with
    | [] -> cfg
    | x :: xs -> let stupdate v (callStack, y :: ys, (s, i, o, r))  = (callStack, ys, (Language.State.update v y s, i, o, r)) in
                 addArgs xs (stupdate x cfg)
				 
let rec eval env ((cstack, stack, ((st, i, o, r) as c)) as conf) prg = 
         match prg with
        | [] -> conf
        | p :: ps -> match p with
            | BINOP binop -> (match stack with
                | x :: y :: xs ->  eval env (cstack, (Expr.to_func binop y x) :: xs, c) ps
                | _ -> failwith "eval BINOP failed!"
			)
			| CONST n -> eval env (cstack, n :: stack, c) ps
			| READ -> (match i with
                | i :: inp ->  eval env (cstack, i :: stack, (st, inp, o, r)) ps
                | _ -> failwith "eval READ failed!"
			)
			| WRITE -> (match stack with
                | x :: xs ->  eval env (cstack, xs, (st, i, o @ [x], r)) ps
                | _ -> failwith "eval WRITE failed!"
			)
			| LD var -> eval env (cstack, (State.eval st var) :: stack, c) ps
			
			| ST var -> (match stack with
                | x :: xs ->  eval env (cstack, xs, ((State.update var x st), i, o, r)) ps
                | _ -> failwith "eval LD failed!"
			)
			| LABEL l -> eval env conf ps
			| JMP l   -> eval env conf (env#labeled l)
			| CJMP(cond, l) -> (match stack with
				    | [] -> conf
					| y :: ys -> if ((cond = "z") = (y = 0)) then eval env (cstack, ys, c) (env#labeled l) else eval env (cstack, ys, c) ps
				)
		   | CALL funcName -> eval env ((ps, st)::cstack, stack, c) (env#labeled funcName)
		   | BEGIN(args, locals) -> let c' = State.enter st (args @ locals) in
                                    eval env (addArgs args (cstack, stack, (c', i, o, r))) ps (* some function names were changed*)
				
		   | END -> (match cstack with
                | [] -> conf
                | (var, st') :: stck -> eval env (stck, stack, (State.leave st st', i, o, r)) var
				)
				 
       | _ -> failwith "eval unknown instruction!"
				
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
(*  | Stmt.Until (c, st)   -> let lp, n = compile' (l + 1) st in
         [LABEL (label_int l)] 
		 @ lp 
		 @ (expr c) 
		 @ [CJMP ("z", (label_int l))], n
		 *)
(* TODO: Old version from hw6 doesn't work and it's code is bad , need to fix *)

class labelMaker =
 object (self)
   val mutable count = 0
   method nextLabel = count <- count + 1; Printf.sprintf "L%d" count
 end
 
let label_maker = new labelMaker
let rec compile (defs, p) =
 let rec compile' = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> compile' x @ compile' y @ [BINOP op]
  | Expr.Call (func, args) -> List.concat (List.map compile' (List.rev args)) @ [CALL func]
  in
  let rec compile'' stmt = match stmt with
  | Stmt.Seq (s1, s2)  -> compile'' s1 @ compile'' s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> compile' e @ [WRITE]
  | Stmt.Assign (x, e) -> compile' e @ [ST x]
  | Stmt.Skip          -> []
  | Stmt.While (c, st) -> let start = label_maker#nextLabel in let endl = label_maker#nextLabel in
         [LABEL start] 
		 @ (compile' c) 
		 @ [CJMP ("z", endl)] 
		 @ compile'' st
		 @ [JMP start] 
		 @ [LABEL endl]
  | Stmt.If (c, tr, fl) -> let elsel = label_maker#nextLabel in let endl = label_maker#nextLabel in
						   (compile' c)
						   @ [CJMP ("z", elsel)] 
						   @ compile'' tr
						   @ [JMP endl] 
						   @ [LABEL elsel] 
						   @ compile'' fl
						   @ [LABEL endl]
  | Stmt.Call (funcName, vars) ->  (List.concat (List.map compile' vars)) @ [CALL funcName]
  | Stmt.Repeat (bl, cond)   -> let start = label_maker#nextLabel in
                           [LABEL start] 
						   @ compile'' bl 
						   @ (compile' cond) 
						   @ [CJMP ("z", start)]
  | Stmt.Return None     -> [END]
  | Stmt.Return (Some r) -> (compile' r) @ [END]
    in 
  let compileF(funcName, (args, locals, body)) = 
    [LABEL funcName; BEGIN (args, locals)]
    @ compile'' body 
    @ [END]
  in
  let mainL = label_maker#nextLabel in
  [JMP mainL]
  @ List.concat (List.map compileF defs) 
  @ [LABEL mainL]
  @ (compile'' p)
 
