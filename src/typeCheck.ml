open Util
open SourceAst

(* Types *)
type t = Tint | Tbool

(* Map identifiers to their types *)
type env_t = t Strmap.t

(* Compute the type of an expression, or raise BadInput if there is a type error *)
let rec type_exp (env : env_t) (e : exp) : t =
  match e with
  | Ident (i, ln) -> 
    (try Strmap.find i env
     with Not_found -> 
       raise (BadInput ("Uninitialised variable " ^ i ^
                        " on line " ^ string_of_int ln)))
  | Num n -> Tint
  | Bool b -> Tbool
  | Oper (e1, op, e2) ->
    let t1 = type_exp env e1 in
    let t2 = type_exp env e2 in
    (match (t1, op, t2) with
     | (Tbool, ((T.And | T.Or), _), Tbool) -> Tbool
     | (Tint, ((T.Plus | T.Minus | T.Times | T.Div | T.Lshift | T.BitOr), _), Tint) -> Tint
     | (Tint, ((T.Lt | T.Eq), _), Tint) -> Tbool
     | (_, (op, ln), _) -> 
       raise (BadInput ("Type error on operator " ^ T.show_op op ^
                        " on line " ^ string_of_int ln)))

(* Type check an identifier being assigned to.
   If it is already assigned, check its type. If it has not been assigned,
   extend the type environment *)
let type_lhs_ident (env :env_t) (x : id) (t : t) (ln : int) : env_t =
  try 
    if Strmap.find x env = t then
      env
    else
      raise (BadInput ("Bad type for " ^ x ^ " on line " ^ string_of_int ln))
  with Not_found -> 
    Strmap.add x t env

(* Type check a list of statements. Raise BadInput if there is an error.  Check
   a list so that earlier assignment statements can extend the environment for
   later statements *)
let rec type_stmts (env :env_t) (stmts : stmt list) : unit = 
  match stmts with
  | [] -> ()
  | In (x, ln) :: stmts' ->
    let env' = type_lhs_ident env x Tint ln in
    type_stmts env' stmts'
  | Out (x, ln) :: stmts' ->
    if Strmap.find x env = Tint then
      type_stmts env stmts'
    else
      raise (BadInput ("Output not an integer on line " ^ string_of_int ln))
  | Assign (x, e, ln) :: stmts' ->
    let t = type_exp env e in
    let env' = type_lhs_ident env x t ln in
    type_stmts env' stmts'
  | While (e, s, ln) :: stmts ->
    if type_exp env e = Tbool then
      (type_stmts env [s];
       type_stmts env stmts)
    else
      raise (BadInput ("While test not of type bool on line" ^ string_of_int ln))
  | Ite (e, s1, s2, ln) :: stmts ->
    if type_exp env e = Tbool then
      (type_stmts env [s1];
       type_stmts env [s2];
       type_stmts env stmts)
    else
      raise (BadInput ("If test not of type bool on line " ^ string_of_int ln))
  | Stmts (s_list, ln) :: stmts' ->
    type_stmts env s_list;
    type_stmts env stmts'
