open Util
open SourceAst

(* Statically evaluate an expression according to the identifier values in env *)
let rec prop_exp (env : exp Strmap.t) (exp : exp) : exp = 
  match exp with
  | Ident (i, ln) -> 
    (try Strmap.find i env 
     with Not_found -> exp)
  | Num n -> Num n
  | Bool b -> Bool b
  | Oper (e1, op, e2) ->
    let o1 = prop_exp env e1 in
    let o2 = prop_exp env e2 in
    (match (o1, op, o2) with
     | (Num n1, (T.Plus, _), Num n2) -> Num (Int64.add n1 n2)
     | (Num n1, (T.Minus, _), Num n2) -> Num (Int64.sub n1 n2)
     | (Num n1, (T.Times, _), Num n2) -> Num (Int64.mul n1 n2)
     | (Num n1, (T.Div, _), Num n2) when n2 <> 0L -> Num (Int64.div n1 n2)
     | (Num n1, (T.Lt, _), Num n2) -> Bool (n1 < n2)
     | (Bool b1, (T.And, _), Bool b2) -> Bool (b1 && b2)
     | (Bool b1, (T.Or, _), Bool b2) -> Bool (b1 || b2)
     | (Bool b1, (T.Eq, _), Bool b2) -> Bool (b1 = b2)
     | (Num n1, (T.Eq, _), Num n2) -> Bool (Int64.compare n1 n2 = 0)
     | _ -> Oper (o1, op, o2))

let is_const (e : exp) : bool =
  match e with
  | Num _ | Bool _ -> true
  | _ -> false

(* Do constant propagation *)
let rec prop_stmts (env : exp Strmap.t) (stmts : stmt list) : stmt list = 
  match stmts with
  | [] -> []
  | In (x,ln) :: stmts -> In (x, ln) :: prop_stmts env stmts
  | Out (x,ln) :: stmts -> Out (x, ln) :: prop_stmts env stmts
  | Assign (x, e, ln) :: stmts -> 
    let o1 = prop_exp env e in
    if is_const o1 then
      Assign (x, o1, ln) :: prop_stmts (Strmap.add x o1 env) stmts
    else
      Assign (x, o1, ln) :: prop_stmts (Strmap.remove x env) stmts
  | While (e, s, ln) :: stmts ->
    let o1 = prop_exp env e in
    let os = prop_stmt env s in
    (match o1 with
     | Bool false -> prop_stmts env stmts
     | _ -> While (o1, os, ln) :: prop_stmts env stmts)
  | Ite (e, s1, s2, ln) :: stmts ->
    let o1 = prop_exp env e in
    let os1 = prop_stmt env s1 in
    let os2 = prop_stmt env s2 in
    (match o1 with
     | Bool true -> os1 :: prop_stmts env stmts
     | Bool false -> os2 :: prop_stmts env stmts
     | _ -> Ite (o1, os1, os2, ln) :: prop_stmts env stmts)
  | Stmts (stmts', ln) :: stmts ->
    Stmts (prop_stmts env stmts', ln) :: prop_stmts env stmts
and prop_stmt env stmt =
  match prop_stmts env [stmt] with
  | [os] -> os
  | _ -> raise (InternalError "prop_stmt")

