(*
   CatML Abstract Syntax
   ----------------------
*)

(* FUN: t1 -> t2 | PAIR: (t1, t2) *)
type arith_type = INT | BOOL | STRING | PAIR of arith_type * arith_type | FUN of arith_type * arith_type

let rec string_to_arith_type s : arith_type = INT

type var = string
type expr =
     (* boolean expression forms *)
     | Bool of bool | And of expr * expr | Or of expr * expr | Not of expr
     (* arithmetic expression forms *)
     | Int of int | Plus of expr * expr | Mult of expr * expr | Equal of expr * expr
     (* string expresison forms *)
     | String of string | Concat of expr * expr
     (* casting *)
     | Cast of arith_type * expr
     (* conditional expression form *)
     | If of expr * expr * expr
     (* variables and let expressions, arith_type is the type of the variable. *)
     | Var of var | Let of arith_type * var * expr * expr
     (* pairs *)
     | Pair of expr * expr | First of expr | Second of expr
     (* functions & recursive functions, the first two arith_types are the return type and parameter type respectively *)
     | Fun of arith_type * arith_type * var * expr | Fix of arith_type  * arith_type * var * var * expr | App of expr * expr
     (* print outputs a string to the terminal, input requests user input from the terminal and reduces into a string containing the user's input *)
     | Print of expr | Input (* Try making it so that input expression can have an int counter so that when a trace is done, the values for each input from a previous run can be reused*)
     (* rand reduces to a random integer in the given range *)
     | Rand of expr * expr
     (* breaks an expression, only the second expression is returned when a break statement is reduced *)
     | Break of expr * expr

let rec type_matches(arith_type, expr) : bool =
  begin match arith_type, expr with
    | INT, Int _ -> true
    | BOOL, Bool _ -> true
    | STRING, String _ -> true
    | PAIR(t1,t2), Pair(e1,e2) -> type_matches(t1,e1) && type_matches(t2,e2)
    | FUN(t1,t2), Fun(t3,t4,_,_) -> t1 = t4 && t2 = t3
    | _, _ -> false
  end

(*
  is_val : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let is_val e = match e with 
     Int _ -> true
   | Bool _ -> true
   | Pair _ -> true
   | Fun _ -> true
   | _ -> false

(*************************)
(* Operational Semantics *)
(*************************)

(* Define substitution *)
let rec subst (x, v, e) : expr =
  match e with
  | Var y -> if x = y then v else (Var y)
  | Let(yt,y,e1,e2) -> Let(yt,y,subst(x,v,e1), if x = y then e2 else subst(x,v,e2))
  | Fun(rt,yt,y,e') -> Fun(rt,yt,y, if x = y then e' else subst(x,v,e'))
  | Fix(rt,yt,f,y,e') -> Fix(rt,yt,f,y, if x = y then e' else subst(x,v,e'))
  | App(e1,e2) -> App(subst(x,v,e1), subst(x,v,e2))
  | Pair(e1,e2) -> Pair(subst(x,v,e1), subst(x,v,e2))
  | First(e') -> First(subst(x,v,e'))
  | Second(e') -> Second(subst(x,v,e'))
  | Bool _ | Int _ | String _ | Input -> e
  | Print(e') -> Print(subst(x,v,e'))
  | Cast(t,e') -> Cast(t,subst(x,v,e'))
  | Concat(e1,e2) -> Concat(subst(x,v,e1), subst(x,v,e2))
  | And(e1,e2) -> And(subst(x,v,e1), subst(x,v,e2))
  | Or(e1,e2) -> Or(subst(x,v,e1), subst(x,v,e2))
  | Not(e') -> Not(subst(x,v,e'))
  | Plus(e1,e2) -> Plus(subst(x,v,e1), subst(x,v,e2))
  | Mult(e1,e2) -> Mult(subst(x,v,e1), subst(x,v,e2))
  | Equal(e1,e2) -> Equal(subst(x,v,e1), subst(x,v,e2))
  | If(e0,e1,e2) -> If(subst(x,v,e0), subst(x,v,e1), subst(x,v,e2))
  | Rand(e1,e2) -> Rand(subst(x,v,e1), subst(x,v,e2))
  | Break(e1,e2) -> Break(subst(x,v,e1), subst(x,v,e2))

type step_result = Step of expr | NotReducible of expr
(* 
   step : expr -> step_result
   in : An expression [[e]]
   out : An expression [[e']] if e -> e' in the operational semantics;
         if no such [[e']] exists, returns [[NotReducible]]
*)
let rec step (e0 : expr) : step_result = 
   match e0 with
   | Bool _ | Int _ | String _ -> NotReducible e0
   | Not (Bool b) -> Step (Bool (not b))
   | Not e1 ->
      begin match step e1 with
       | Step e1' -> Step (Not e1')
       | NotReducible e1' -> NotReducible e1'
      end
   | And (Bool b1, Bool b2) -> Step (Bool (b1 && b2))
   | And (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (And (e1', e2))
       | NotReducible e1 ->
          begin match step e2 with
           | Step e2' -> Step (And (e1, e2'))
           | NotReducible e2' -> NotReducible e2'
          end
       end
   | Or (Bool b1, Bool b2) -> Step (Bool (b1 || b2))
   | Or (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Or (e1', e2))
       | NotReducible e1 ->
          begin match step e2 with
           | Step e2' -> Step (Or (e1, e2'))
           | NotReducible e2' -> NotReducible e2'
          end
       end
   | Plus (Int n1, Int n2) -> Step (Int (n1 + n2))
   | Plus (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Plus (e1', e2))
       | NotReducible e1 ->
          begin match step e2 with
           | Step e2' -> Step (Plus (e1, e2'))
           | NotReducible e2' -> NotReducible e2'
          end
       end
   | Mult (Int n1, Int n2) -> Step (Int (n1 * n2))
   | Mult (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Mult (e1', e2))
       | NotReducible e1 ->
           begin match step e2 with
           | Step e2' -> Step (Mult (e1, e2'))
           | NotReducible e2' -> NotReducible e2'
           end
       end
   | Equal (Int n1, Int n2) -> Step (Bool (n1 = n2))
   | Equal (e1, e2) ->
      begin match step e1 with
       | Step e1' -> Step (Equal (e1', e2))
       | NotReducible e1 ->
          begin match step e2 with
           | Step e2' -> Step (Equal (e1, e2'))
           | NotReducible e2' -> NotReducible e2'
          end
       end
   | If (Bool true, e1, _) -> Step e1
   | If (Bool false, _, e2) -> Step e2
   | If (e1, e2, e3) ->
      begin match step e1 with
       | Step e1' -> Step (If (e1', e2, e3))
       | NotReducible e1' -> NotReducible e1'
      end
   | Var _ -> NotReducible e0
   | Let(xt,x,e1,e2) -> 
      begin match step e1 with
      | Step e1' -> Step (Let(xt,x,e1',e2))
      | NotReducible e1' -> if type_matches(xt,e1) then Step(subst(x,e1,e2)) else NotReducible e1'
      end
   | Pair(e1,e2) ->
      begin match step e1 with
      | Step e1' -> Step (Pair(e1',e2))
      | NotReducible e1 ->
        begin match step e2 with
          | Step e2' -> Step (Pair(e1,e2'))
          | NotReducible e2 -> NotReducible e2
        end
      end
   | First(Pair(e1, e2)) -> Step (e1)
   | First(e) -> 
      begin match step e with
        | Step e' -> Step (First(e'))
        | NotReducible e' -> NotReducible e'
      end
   | Second(Pair(e1,e2)) -> Step (e2)
   | Second(e) ->
      begin match step e with
        | Step e' -> Step (Second(e'))
        | NotReducible e' -> NotReducible e'
      end
   | Fun _ -> NotReducible e0
   | Fix(ft,yt,f,y,e) -> Step (Fun(ft,yt,y,subst(f, Fix(ft,yt,f,y,e), e)))
   | App(e1, e2) ->
      begin match step e1 with
        | Step e1' -> Step (App(e1',e2))
        | NotReducible e1' ->
        begin match e1 with
          | Fun(ft,yt,y,e3) -> Step (subst(y,e2,e3))
          | Fix(ft,yt,f,y,e3) -> Step (App(Fun(ft,yt,y, subst(f, e1, e3)), e2))
          | _ -> NotReducible e1
        end
      end
   | Input -> print_string(">>> "); Step (String(read_line()))
   | Print(e) -> 
      begin match step e with
        | Step e' -> Step (Print(e'))
        | NotReducible e' ->
        begin match e with
          | String(s) -> print_string(s ^ "\n"); NotReducible e
          | _ -> NotReducible e
        end
      end
   | Cast(t, e) ->
      begin match step e with
        | Step e' -> Step (Cast(t, e'))
        | NotReducible e' ->
        begin match t, e with
            | INT, Int i -> Step (Int(i))
            | INT, Bool b -> Step (Int(if b then 1 else 0))
            | INT, String s -> (try Step (Int(int_of_string(s))) with _ -> NotReducible e0)
            | BOOL, Int i -> Step (Bool(i > 0))
            | BOOL, Bool b -> Step (Bool(b))
            | BOOL, String s -> if (s == "True" || s == "False") then Step(Bool(s == "True")) else NotReducible e0
            | STRING, Int i -> Step (String(string_of_int(i)))
            | STRING, Bool b -> Step (String(string_of_bool(b)))
            | STRING, String s -> Step (String(s))
            | _, _ -> NotReducible e'
        end
      end
   | Concat(e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Concat(e1',e2))
      | NotReducible e1' ->
        begin match e1, step e2 with
        | String _, Step e2' -> Step (Concat(e1,e2'))
        | String s1, NotReducible e2' ->
            begin match e2 with
            | String s2 -> Step (String(s1 ^ s2))
            | _ -> NotReducible e2'
            end
        | _, _ -> NotReducible e1'
        end
      end
   | Rand(e1, e2) -> 
     begin match step e1 with
     | Step e1' -> Step (Rand(e1', e2))
     | NotReducible e1' ->
        begin match e1, step e2 with
        | Int _, Step e2' -> Step (Rand(e1, e2'))
        | Int i1, NotReducible e2' ->
            begin match e2 with
            | Int i2 -> Random.self_init(); Step (Int(Random.int_in_range ~min:i1 ~max:i2))
            | _ -> NotReducible e2'
            end
        | _, _ -> NotReducible e1'
        end
     end
   | Break(e1, e2) ->
     begin match step e1 with
     | Step e1' -> Step (Break(e1',e2))
     | NotReducible _ -> Step(e2)
     end


(*
   multistep : expr -> expr
   in : AST [[e]]
   out : [[e']] such that e ->* e' -/-> in the operational semantics
*)
let rec multistep e = match step e with 
    | Step e' -> multistep e'
    | NotReducible e' -> e

let rec eval (e : expr) : step_result =
    NotReducible(Bool false)
;;

(*****************)
(* Type checking *)
(*****************)

(* Define a type to represent finite maps/typing contexts *)
type context = (var * arith_type) list

(* empty_context : context *)
let empty_context : context = []

(* lookup : context * var -> arith_type option
 * input :
 *    g - a typing context
 *    x - a variable
 * output :
 *    if (x,tau) is in g, return Some tau
 *    otherwise, return None
 *)
let rec lookup (g,x) : arith_type option =
  match g with
  | [] -> None
  | (y,tp)::g' -> if x = y then Some tp else lookup(g',x)

(* extend : context * var * arith_type -> context
 * input:
 *    g - a typing context
 *    x - a variable
 *    tp - an arith_type
 * output:
 *    A typing context that maps x to tp and any other y to g(y)
 *)
let extend (g,x,tp) : context = (x,tp)::g

type type_result = Type of arith_type | NoType of expr

let rec type_of_expr (g, e0) : type_result =
  match e0 with
  | Var x ->
    begin match lookup(g,x) with
    | Some t -> Type t
    | None -> NoType e0
    end
  | Let(xt,x,e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type t -> type_of_expr (extend(g,x,t), e2)
    | NoType e' -> NoType e'
    end
  | Bool _ -> Type BOOL
  | And(e1,e2) | Or(e1,e2) -> 
    begin match type_of_expr(g,e1) with
    | Type BOOL -> type_of_expr(g,e2)
    | NoType e'-> NoType e'
    | _ -> NoType e0
    end
  | Not(e) ->
    begin match type_of_expr (g,e) with
    | Type BOOL -> Type BOOL
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | Int _ -> Type INT
  | Plus(e1,e2) | Mult(e1,e2) -> 
    begin match type_of_expr (g,e1) with
    | Type INT ->
        begin match type_of_expr (g,e2) with
        | Type INT -> Type INT
        | NoType e' -> NoType e'
        | _ -> NoType e1
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | Equal(e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type INT -> 
        begin match type_of_expr (g,e2) with
        | Type INT -> Type BOOL
        | NoType e' -> NoType e'
        | _ -> NoType e1
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | If(e1,e2,e3) ->
    begin match type_of_expr (g,e1) with
    | Type BOOL ->
        begin match type_of_expr (g,e2) with
        | Type t1 ->
            begin match type_of_expr (g,e3) with
            | Type t2 -> if t1 = t2 then Type t1 else NoType e0
            | NoType e' -> NoType e'
            end
        | NoType e' -> NoType e'
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | Pair(e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type t1 ->
        begin match type_of_expr (g,e2) with
        | Type t2 -> if t1 = t2 then Type t1 else NoType e0
        | NoType e' -> NoType e'
        end
    | NoType e' -> NoType e'
    end
  | First(e) -> 
    begin match type_of_expr (g,e) with
      | Type PAIR(t1,t2) -> Type t1
      | _ -> NoType e0
    end
  | Second(e) ->
    begin match type_of_expr (g,e) with
      | Type PAIR(t1,t2) -> Type t2
      | _ -> NoType e0
    end
  | Fun(ft,yt,y,e) -> Type (FUN(ft,yt))
  | Fix(ft,yt,f,y,e) -> Type (FUN(ft,yt))
  | App(e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type FUN(ft,yt) -> 
        begin match type_of_expr (g,e2) with 
        | Type xt -> if xt = yt then Type ft else NoType e0
        | NoType e' -> NoType e'
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | String _ -> Type STRING
  | Input -> Type STRING
  | Print _ -> NoType e0
  | Cast(t,e) ->
    begin match type_of_expr (g,e) with
      | Type INT | Type BOOL | Type STRING -> Type t
      | NoType e' -> NoType e'
      | _ -> NoType e0
    end
  | Concat(e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type STRING ->
        begin match type_of_expr (g,e2) with
        | Type STRING -> Type STRING
        | NoType e' -> NoType e'
        | _ -> NoType e0
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | Rand(e1,e2) ->
    begin match type_of_expr (g,e1) with
    | Type INT ->
        begin match type_of_expr (g,e2) with
        | Type INT -> Type INT
        | NoType e' -> NoType e'
        | _ -> NoType e0
        end
    | NoType e' -> NoType e'
    | _ -> NoType e0
    end
  | Break(e1,e2) -> type_of_expr (g,e2)
;;

(********************************************)
(* Utility functions for working with ARITH *)
(********************************************)

let rec string_of_type t : string =
  match t with
    | Some INT -> "int"
    | Some BOOL -> "bool"
    | Some STRING -> "string"
    | Some PAIR(t1,t2) -> "(" ^ string_of_type (Some t1) ^ "," ^ string_of_type (Some t2) ^ ")"
    | Some FUN(t1,t2) -> "fun(" ^ string_of_type (Some t1) ^ "->" ^ string_of_type (Some t2) ^ ")"
    | None -> "None"
;;

(*
  string_of_expr : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec string_of_expr e = match e with
   | Var x -> x
   | Let(xt,x,e1,e2) -> "Let " ^ x ^ ":" ^ string_of_type(Some xt) ^ " = " ^ string_of_expr e1 ^ " In " ^ string_of_expr e2
   | Bool true -> "True"
   | Bool false -> "False"
   | Int n -> string_of_int n
   | And (e1, e2) -> "(" ^ (string_of_expr e1) ^ " And " ^ (string_of_expr e2) ^ ")"
   | Or (e1, e2) -> "(" ^ (string_of_expr e1) ^ " Or " ^ (string_of_expr e2) ^ ")"
   | Not e1 -> "(Not " ^ (string_of_expr e1) ^ ")"
   | Plus (e1, e2) -> "(" ^ (string_of_expr e1) ^ " + " ^ (string_of_expr e2) ^ ")"
   | Mult (e1, e2) -> "(" ^ (string_of_expr e1) ^ " x " ^ (string_of_expr e2) ^ ")"
   | Equal (e1, e2) -> "(" ^ (string_of_expr e1) ^ " = " ^ (string_of_expr e2) ^ ")"
   | If(e1, e2, e3) -> "If " ^ (string_of_expr e1) ^ 
                       " Then " ^ (string_of_expr e2) ^
                       " Else " ^ (string_of_expr e3)
   | Pair (e1, e2) -> "(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
   | First (e) -> "(First " ^ (string_of_expr e) ^ ")"
   | Second (e) -> "(Second " ^ (string_of_expr e) ^ ")"
   | Fun (ft, yt, y, e) -> "(Fun " ^ string_of_type(Some ft) ^ " " ^ y ^ ":" ^ string_of_type(Some yt) ^ " -> " ^ string_of_expr(e) ^ ")"
   | Fix (ft, yt, f, y, e) -> "(Fix " ^ string_of_type(Some ft) ^ " " ^ f ^ "." ^ y ^ ":" ^ string_of_type(Some yt) ^ " -> " ^ string_of_expr(e) ^ ")"
   | App (e1, e2) -> (string_of_expr e1) ^ "(" ^ (string_of_expr e2) ^ ")"
   | String s -> "\"" ^ s ^ "\""
   | Input -> "Input"
   | Print e' -> "(Print " ^ (string_of_expr e') ^ ")"
   | Cast (t, e) -> "(" ^ string_of_type(Some t) ^ ") " ^ string_of_expr(e)
   | Rand(e1, e2) -> "Rand(" ^ (string_of_expr(e1)) ^ ", " ^ (string_of_expr(e2)) ^ ")"
   | Concat(e1, e2) -> (string_of_expr e1) ^ " ^ " ^ (string_of_expr e2)
   | Break(e1,e2) -> (string_of_expr e1) ^ ";" ^ (string_of_expr e2)
;;

(*
  pretty_trace : expr -> bool -> unit
  in : AST [[e]], stepper flag 
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions if stepper is on.
*)
let rec pretty_trace e stepper =
   Printf.printf "%s" (string_of_expr e); if stepper then ignore (read_line()) else ();
   if (is_val e) then (Printf.printf "\n"; flush stdout) else
    match step e with
    | NotReducible e' -> Printf.printf "  (Bad, Stuck Expression)\n"; flush stdout
    | Step e' -> Printf.printf "->\n"; flush stdout; pretty_trace e' stepper

let rec pretty_trace_2 e stepper : string =
    if (is_val e) then (string_of_expr e) ^ "\n" else
    match step e with
        | NotReducible e' -> (string_of_expr e) ^ "  (Bad, Stuck Expression)\n"
        | Step e' -> (string_of_expr e) ^ "->\n" ^ (pretty_trace_2 e' stepper)

let rec trace_and_reduce e stepper : expr * string =
    if (is_val e) then (e, (string_of_expr e) ^ "\n") else
    begin match step e with
        | NotReducible e' -> (e', (string_of_expr e) ^ "  (Bad, Stuck Expression)\n")
        | Step e' ->
            let result, trace = trace_and_reduce e' stepper in
            (result, (string_of_expr e) ^ "->\n" ^ trace)
    end

(*
  stepper : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions.
*)
let rec stepper e = pretty_trace e true;;
(*
  tracer : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. 
*)
let rec tracer e = pretty_trace e false;;

(* Testing *)
(*let test_expr = 
  Break(Print(String("Wow\n")),
  Let(INT, "v", StringToInt(Input),
  Let(FUN(INT, INT), "sum",
    Fix(INT, INT, "rec", "x",
      If(
        Equal(Var("x"), Int(0)),
        Int(0),
        Plus(Var("x"), App(Var("rec"), Plus(Var("x"), Int(-1))))
      )
    ),
    App(Var("sum"), Var("v"))
  ))
  )
;;

print_string(string_of_expr(multistep(test_expr)) ^ "\n");;*)

(*print_string(string_of_expr(test_expr))*)