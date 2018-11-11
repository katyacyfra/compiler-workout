(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns a pair: the return value for the call and the resulting configuration
    *) 
	  let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
	 
	let concatResults (conf1, res1) (conf2, res2) = (conf2, res1::res2) 
	let rec evalArgs env ((st, i, o, n) as conf) exprs = match exprs with
      | [] -> (conf, [])
      | x::xs -> let (st', i', o', Some r') = eval env conf x in concatResults (conf, r') (evalArgs env (st', i', o', Some r') xs)
	  
    and
	eval env ((st, i, o, r) as conf) expr = 
	match expr with
	   | Var x -> (st, i, o, Some (State.eval st x))
	   | Const x -> (st, i, o, Some x)
	   | Binop (binop, l, r) -> let ((st', i', o', Some l') as conf') = eval env conf l in
          let ((st'', i'', o'', Some r') as conf) = eval env conf' r in (st'', i'', o'', Some (to_func binop l' r'))  (* Left to right! *)
	   | Call (funcname, args) -> let (newConf, results) = evalArgs env conf args in env#definition env funcname results newConf
	   
	   

         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
    ostap (                                      
           parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
		 
		 
      
      primary:
	   funcname:IDENT -"(" args:!(Ostap.Util.list0)[parse] -")" { Call (funcname, args) } 
      | n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
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
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show
           
		   
    let toSeq x y = match (x, y) with 
	  | x, Skip -> x
      | Skip, y -> y
      | x, y -> Seq (x, y)
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
	
    let rec eval env ((st, i, o, r) as conf) k stmt =
	   match stmt with
	     | Read var -> (match i with
			    | [] -> failwith "Empty input!"
				| x :: xs -> eval env (State.update var x st, xs, o, None) Skip k
				)
		 | Write expr -> let (state, inp, out, Some r') = Expr.eval env conf expr in eval env (state, inp, out @ [r'], None) Skip k (* (state, input, output @ [(Expr.eval state expr)]) *)
         | Assign(var, exp) -> let (state, inp, out, Some r') = Expr.eval env conf exp in eval env (State.update var r' state, inp, out, None) Skip k
		 | Seq(firstst, secondst) -> eval env conf (toSeq secondst k) firstst
		 | If(cond, tr, fl) -> let (state, inp, out, Some r') = Expr.eval env conf cond in eval env (state, inp, out, None) k (if r' <> 0 then tr else fl)
		 | While(cond, stm) -> let (state, inp, out, Some r') = Expr.eval env conf cond in 
		        (if r' <> 0 then eval env (state, inp, out, None) (toSeq (While (cond, stm)) k) stm else eval env (state, inp, out, None) Skip k)
	     | Skip -> (match k with 
		        | Skip -> conf 
				| k -> eval env conf Skip k
				)
		 | Repeat (block, stm) -> eval env conf (toSeq (While (Expr.Binop ("==", stm, Expr.Const 0), block)) k) block
		 | Call(name, args) -> let (state, inp, out, r') = Expr.eval env conf (Expr.Call (name, args)) in eval env (state, inp, out, r') k Skip
		 | Return result -> (match result with 
		        | None -> conf 
				| Some res -> Expr.eval env conf res)
				
	
	
	  let default x opt = match opt with
        | Some v -> v
        | None   -> x	

    (* Statement parser *)
    ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.parse)                                               { Assign(x, e) }
      | "read" "(" x:IDENT ")"                                                     { Read x } 
      | "write" "(" e:!(Expr.parse) ")"                                            { Write e }   
	  | "skip"                                                                     { Skip }
	  | "while" c:!(Expr.parse) "do" st:stmt "od"                                  { While(c, st) }
	  | "for" init:stmt "," c:!(Expr.parse) "," incr:stmt "do" st:stmt "od"        { Seq (init, While(c, Seq(st, incr))) }
	  | "repeat" st:stmt "until" c:!(Expr.parse)                                   { Repeat (st, c) }
	  | "if" c:!(Expr.parse) "then" tr:stmt "elif" elif:elseif "fi"                { If (c, tr, elif) }
      | "if" c:!(Expr.parse) "then" tr:stmt "else" fl:stmt "fi"                    { If (c, tr, fl) }
      | "if" c:!(Expr.parse) "then" tr:stmt "fi"                                   { If (c, tr, Skip) }
	  | funcName:IDENT "(" args:(!(Util.list)[ostap (!(Expr.parse))])? ")"         { Call (funcName, default [] args)}
      | "return" e:!(Expr.parse)                                                   { Return (Some e) }
      | "return"                                                                   { Return None };
	  
	  arguments: 
        a:!(Expr.parse)                                                            { a :: [] }
      | a:!(Expr.parse) -"," ax:!(arguments)                                       { a :: ax };
	  
	  elseif: 
	  	c:!(Expr.parse) "then" tr:stmt "elif" elif:elseif                          { If (c, tr, elif) }
      | c:!(Expr.parse) "then" tr:stmt "else" fl:stmt                              { If (c, tr, fl) }
      | c:!(Expr.parse) "then" tr:stmt                                             { If (c, tr, Skip) };


      stmt: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss};
      parse: stmt)
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =
           let xs, locs, s      = snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
