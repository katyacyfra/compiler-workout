(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
       
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
        (* Implementation of casting functions *)
    let boolToNumber x = if x then 1 else 0
    let numberToBool x = if (x <> 0) then true else false
	
	let binOp op left right = match op with
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | "/" -> left / right
        | "%" -> left mod right
        | "<" -> boolToNumber (left < right)
        | "<=" -> boolToNumber (left <= right)
        | ">" -> boolToNumber (left > right)
        | ">=" -> boolToNumber (left >= right)
        | "==" -> boolToNumber (left = right)
        | "!=" -> boolToNumber (left <> right)
        | "&&" -> boolToNumber (numberToBool left && numberToBool right)
        | "!!" -> boolToNumber (numberToBool left || numberToBool right)
        | _ -> failwith("Unknown operator!")
	

    (* Implementation of eval *)
    let rec eval state expr = 
        match expr with
        | Var x -> state x
        | Const n -> n 
        | Binop (op, l, r) -> binOp op (eval state l) (eval state r)
		
let binOp op = fun l r -> Binop (op, l, r)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
ostap (
      expr:
        !(Util.expr
           (fun x -> x)
           [|
             `Lefta , [ostap ("!!"), (binOp "!!"); 
					   ];
             `Lefta , [ostap ("&&"), (binOp "&&"); 
					   ];
             `Nona  , [ostap ("<="), (binOp "<=");
		       ostap (">="), (binOp ">=");
		       ostap (">"), (binOp ">");
		       ostap ("<"), (binOp "<")
			          ];
             `Nona  , [ostap ("=="), (binOp "==");
	               ostap ("!="), (binOp "!=")
			          ];
             `Lefta , [ostap ("+"), (binOp "+"); 
			           ostap ("-"), (binOp "-")
					   ];
             `Lefta , [ostap ("*"), (binOp "*"); 
			           ostap ("/"), (binOp "/");
					   ostap ("%"), (binOp "%")
					  ]
           |]
           primary
		   );
		primary: x:IDENT {Var x} | n:DECIMAL {Const n} | -"(" expr -")"
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
    (* loop with a post-condition       *) | Until  of Expr.t * t  with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
	let rec eval (state, input, output) sttype =
	    match sttype with
		    | Read var -> (match input with
			    | [] -> failwith "Empty input!"
				| x :: xs -> ((Expr.update var x state), xs, output))
			| Write expr -> (state, input, output @ [(Expr.eval state expr)])
			| Assign(var, value) -> ((Expr.update var (Expr.eval state value) state), input, output)
            | Seq(firstst, secondst) -> (eval (eval (state, input, output) firstst) secondst)
			| If(cond, tr, fl) -> if (Expr.eval state cond <> 0) then eval (state, input, output) tr else eval (state, input, output) fl
			| While(cond, st) -> if (Expr.eval state cond <> 0) then eval (eval (state, input, output) st) (While (cond, st)) else (state, input, output)
			| Until (cond, st) -> let (s, i, o) = eval (state, input, output) st in if (Expr.eval s cond <> 0) then (s, i, o) else eval (s, i, o) (Until (cond, st))
			| Skip -> (state, input, output)
                               
    (* Statement parser *)
	(* Don't forget about parsing order!!!!!!!!!!!! *)
    ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.expr)                                               { Assign(x, e) }
      | "read" "(" x:IDENT ")"                                                    { Read x } 
      | "write" "(" e:!(Expr.expr) ")"                                            { Write e }   
	  | "skip"                                                                    { Skip }
	  | "while" c:!(Expr.expr) "do" st:stmt "od"                                  { While(c, st) }
	  | "for" init:stmt "," c:!(Expr.expr) "," incr:stmt "do" st:stmt "od"        { Seq (init, While(c, Seq(st, incr))) }
	  | "repeat" st:stmt "until" c:!(Expr.expr)                                   { Until (c, st) }
	  | "if" c:!(Expr.expr) "then" tr:stmt "elif" elif:elseif "fi"                { If (c, tr, elif) }
      | "if" c:!(Expr.expr) "then" tr:stmt "else" fl:stmt "fi"                    { If (c, tr, fl) }
      | "if" c:!(Expr.expr) "then" tr:stmt "fi"                                   { If (c, tr, Skip) };

	  
	  elseif: 
	  	c:!(Expr.expr) "then" tr:stmt "elif" elif:elseif                          { If (c, tr, elif) }
      | c:!(Expr.expr) "then" tr:stmt "else" fl:stmt                              { If (c, tr, fl) }
      | c:!(Expr.expr) "then" tr:stmt                                             { If (c, tr, Skip) };


      stmt: <s::ss> : !(Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss};
      parse: stmt)
      
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
