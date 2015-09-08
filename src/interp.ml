open Util
open SourceAst

type store_t = Int64.t Strmap.t

let bool_to_int64 (b : bool) : Int64.t = 
  if b then 1L else 0L
let int64_to_bool (i : Int64.t) : bool = 
  if Int64.compare i 0L = 0 then false else true

(* Do a primitive operation *)
let do_op op (n1 : Int64.t) (n2 : Int64.t) : Int64.t = match op with
  | T.Plus -> Int64.add n1 n2
  | T.Minus -> Int64.sub n1 n2
  | T.Times -> Int64.mul n1 n2
  | T.Div -> Int64.div n1 n2
  | T.Lt -> bool_to_int64 (Int64.compare n1 n2 = -1)
  | T.Eq -> bool_to_int64 (Int64.compare n1 n2 = 0)
  | T.And -> bool_to_int64 (int64_to_bool n1 && int64_to_bool n2)
  | T.Or -> bool_to_int64 (int64_to_bool n1 || int64_to_bool n2)
  | T.Lshift -> Int64.shift_left n1 (Int64.to_int n2)
  | T.BitOr -> Int64.logor n1 n2

(* Compute the value of an expression *)
let rec interp_exp (store : store_t) (e : exp) : Int64.t = match e with
  | Ident (i, _) -> Strmap.find i store
  | Num n -> n
  | Bool b -> bool_to_int64 b
  | Oper (e1, (op, _), e2) ->
    do_op op (interp_exp store e1) (interp_exp store e2)

(* Run a statement *)
let rec interp_stmt (store : store_t) (s : stmt) : store_t = match s with
  | Assign (i, e, _) ->
    let n = interp_exp store e in
    Strmap.add i n store
  | While (e, body_s, loc) ->
    if not (int64_to_bool (interp_exp store e)) then
      store
    else
      let store2 = interp_stmt store body_s in
      interp_stmt store2 s
  | Ite (e, s1, s2, _) ->
    if int64_to_bool (interp_exp store e) then
      interp_stmt store s1
    else
      interp_stmt store s2
  | Stmts (sl, _) ->
    interp_stmts store sl
  | In (i, _) ->
    Printf.printf "> ";
    let n = Int64.of_string (read_line ()) in
      Strmap.add i n store
  | Out (i, _) ->
    begin
      print_string (Int64.to_string (Strmap.find i store));
      print_newline ();
      store
    end
and interp_stmts (store : store_t) (sl : stmt list) : store_t = match sl with
  | [] -> store
  | s::sl ->
    let store2 = interp_stmt store s in
    interp_stmts store2 sl;;

interp_stmts Strmap.empty (FrontEnd.front_end false)
