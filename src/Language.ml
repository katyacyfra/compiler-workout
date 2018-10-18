(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap

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
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

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
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
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
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

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
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )
    
  end
  
  let default x opt = match opt with
        | Some v -> v
        | None   -> x	
                    
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
    (* loop with a post-condition       *) | Until  of Expr.t * t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
 
 	let rec argsHelperAssign vs xs st = match (vs, xs) with
      | ([], [])           -> st
      | (v :: vs, x :: xs) -> argsHelperAssign vs xs (State.update x v st)
	
	
    	let rec eval env (state, input, output) sttype =
	    match sttype with
		    | Read var -> (match input with
			    | [] -> failwith "Empty input!"
				| x :: xs -> ((State.update var x state), xs, output))
			| Write expr -> (state, input, output @ [(Expr.eval state expr)])
			| Assign(var, value) -> ((State.update var (Expr.eval state value) state), input, output)
            | Seq(firstst, secondst) -> (eval env (eval env (state, input, output) firstst) secondst)
			| If(cond, tr, fl) -> if (Expr.eval state cond <> 0) then eval env (state, input, output) tr else eval env (state, input, output) fl
			| While(cond, st) -> if (Expr.eval state cond <> 0) then eval env (eval env (state, input, output) st) (While (cond, st)) else (state, input, output)
            | Until (cond, st) -> let (s, i, o) = eval env (state, input, output) st in if (Expr.eval s cond <> 0) then (s, i, o) else eval env (s, i, o) (Until (cond, st))  
			| Skip -> (state, input, output)
			| Call(name, args) -> let (arguments, locals, body) = env#definition name in
			                      let values = List.combine arguments (List.map (Expr.eval state) args) in
								  let newst = State.push_scope state (arguments @ locals) in
                                  let newst' = List.fold_left (fun state (var, vall) -> State.update var vall state) newst values in
                                  let newst'', ni, no = eval env (newst', input, output) body
                                  in (State.drop_scope newst'' state, ni, no)

	                      
                                
    (* Statement parser *)
    ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.parse)                                               { Assign(x, e) }
      | "read" "(" x:IDENT ")"                                                     { Read x } 
      | "write" "(" e:!(Expr.parse) ")"                                            { Write e }   
	  | "skip"                                                                     { Skip }
	  | "while" c:!(Expr.parse) "do" st:stmt "od"                                  { While(c, st) }
	  | "for" init:stmt "," c:!(Expr.parse) "," incr:stmt "do" st:stmt "od"        { Seq (init, While(c, Seq(st, incr))) }
	  | "repeat" st:stmt "until" c:!(Expr.parse)                                   { Until (c, st) }
	  | "if" c:!(Expr.parse) "then" tr:stmt "elif" elif:elseif "fi"                { If (c, tr, elif) }
      | "if" c:!(Expr.parse) "then" tr:stmt "else" fl:stmt "fi"                    { If (c, tr, fl) }
      | "if" c:!(Expr.parse) "then" tr:stmt "fi"                                   { If (c, tr, Skip) }
	  | funcName:IDENT "(" args:(!(Util.list)[ostap (!(Expr.parse))])? ")"         { Call (funcName, default [] args)}; 
	  
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
      parse: 
        -"fun" funcName:IDENT -"(" args:(!(Util.list)[ostap (IDENT)])? -")" localVars:(-"local" !(Util.list)[ostap (IDENT)])? -"{" body:!(Stmt.parse) -"}"
        {(funcName, (default [] args, default [] localVars, body))}
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
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
