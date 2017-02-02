(*
 * Example compiler
 * Copyright (C) 2015-2016 Scott Owens
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(* The type checker *)

open Util
open SourceAst
module T = Tokens

(* Types *)
type t =
  | Tint
  | Tbool
  | Tarray of int (* The number of dimensions that the array has *)

let show_t t =
  match t with
  | Tint -> "int"
  | Tbool -> "bool"
  | Tarray n -> "array " ^ string_of_int n

(* Map functions and identifiers to their types *)
type env_t = { funs : (t list * t) Idmap.t; vars : t Idmap.t }

(* Raise a type error *)
let type_error (ln : int option) (msg : string) : 'a =
  match ln with
  Some ln ->
    raise (BadInput ("Type error on line " ^ string_of_int ln ^ ": " ^ msg))
  | None ->
    raise (BadInput ("Type error at unknown location: " ^ msg))

(* Compute the type of an expression, or raise BadInput if there is a type
   error *)
let rec type_exp (ln : int option) (env : env_t) (e : exp) : t =
  match e with
  | Ident (i, es) ->
    let t =
      try Idmap.find i env.vars
      with Not_found ->
        type_error ln ("Undeclared variable " ^ show_id i)
    in
    let ts = List.map (type_exp ln env) es in
    let l = List.length ts in
    (* If there are no indices, then just use the type of the identifier, even
       if it is of array type. If there are indices, they must fully index the
       array, which always contains ints *)
    if l = 0 then
      t
    else
      (match t with
       | Tarray num_dims ->
         if num_dims = l && List.for_all (fun x -> x = Tint) ts then
           Tint
         else if num_dims = l then
           type_error ln "Array index with non-integer type"
         else
           type_error ln ("Array reference with " ^ string_of_int l ^
                          " indices, expected " ^ string_of_int num_dims)
       | t ->
         type_error ln ("Attempt to index non-array variable " ^ show_id i))
  | Call (f, args) ->
    let (param_types, ret_type) =
      try Idmap.find f env.funs
      with Not_found ->
        type_error ln ("Call to undefined function " ^ show_id f)
    in
    let ts = List.map (type_exp ln env) args in
    let rec check n pts ats =
      match (pts, ats) with
      | ([], []) -> ()
      | (pt::pts, at::ats) ->
        if pt = at then check (n+1) pts ats else
          type_error ln ("function call given argument of type " ^ show_t at ^
                         " but expected type " ^ show_t pt ^
                         " in argument number " ^ string_of_int n)
      | _ -> assert false
    in
    if List.length param_types <> List.length args then
      type_error ln ("function call expects " ^
                     string_of_int (List.length param_types) ^
                     " arguments, given " ^
                     string_of_int (List.length ts))
    else
      check 1 param_types ts;
    ret_type
  | Num n -> Tint
  | Bool b -> Tbool
  | Op (e1, op, e2) ->
    (match (type_exp ln env e1, op, type_exp ln env e2) with
     | (Tbool, (T.And | T.Or), Tbool) -> Tbool
     | (Tint, (T.Plus | T.Minus | T.Times | T.Div | T.Lshift | T.BitOr |
               T.BitAnd), Tint) ->
       Tint
     | (Tint, (T.Lt | T.Eq | T.Gt), Tint) -> Tbool
     | (t1, _, t2) ->
       type_error ln ("Operator " ^ T.show_op op ^ " applied to " ^ show_t t1 ^
                      " and " ^ show_t t2))
  | Uop (uop, e) ->
    (match (uop, type_exp ln env e) with
     | (T.Not, Tbool) -> Tbool
     | (_, t) ->
       type_error ln ("Operator " ^ T.show_uop uop ^ " applied to " ^ show_t t))
  | Array es ->
    let ts = List.map (type_exp ln env) es in
    let l = List.length ts in
    if l = 0 then
      type_error ln "Array must have at least 1 dimension"
    else if List.for_all (fun x -> x = Tint) ts then
      Tarray l
    else
      type_error ln "Array dimension with non-integer type"

(* Type check a statement. Raise BadInput if there is an error. *)
let rec type_stmt (ln : int option) (env :env_t) (return : t) (stmt : stmt) : unit =
  match stmt with
  | In i | Out i ->
    let t = type_exp ln env (Ident(i, [])) in
    if t <> Tint then
      type_error ln "Input/Output with non-integer type"
    else
      ()
  | Return i ->
    let t = type_exp ln env (Ident(i, [])) in
    if t <> return then
      type_error ln ("return has type " ^ show_t t ^ " in a function with return type " ^ show_t return)
    else
      ()
  | Assign (x, es, e) ->
    let t1 = type_exp ln env (Ident (x, es)) in
    let t2 = type_exp ln env e in
    if t1 <> t2 then
      type_error ln "Array assignment type mismatch"
    else
      ()
  | DoWhile (s1, e, s2) ->
    type_stmt ln env return s1;
    if type_exp ln env e = Tbool then
      type_stmt ln env return s2
    else
      type_error ln "Do/while test of non-bool type"
  | Ite (e, s1, s2) ->
    if type_exp ln env e = Tbool then
      (type_stmt ln env return s1;
       type_stmt ln env return s2)
    else
      type_error ln "If test of non-bool type"
  | Stmts (s_list) ->
    List.iter (type_stmt ln env return) s_list
  | Loc (s, ln') ->
    type_stmt (Some ln') env return s

let source_typ_to_t t =
  match t with
  | Int -> Tint
  | Bool -> Tbool
  | Array n -> Tarray n

(* TODO: Add source location and error message *)
let rec type_var_dec_list (env : env_t) (decs : var_dec list) : env_t =
  match decs with
  | [] -> env
  | v::decs ->
    let t = type_exp None env v.init in
    if t = source_typ_to_t v.typ then
      let new_env = { env with vars = Idmap.add v.var_name t env.vars } in
      type_var_dec_list new_env decs
    else
      type_error None ""

(* Check for duplicate parameters. Raise an exception if found *)
let rec check_dup_params (ln : int option) params : unit =
  match params with
  | [] -> ()
  | (i,_) :: params ->
    if List.mem_assoc i params then
      type_error ln ("Duplicate function parameter " ^ show_id i)
    else
      check_dup_params ln params

(* TODO: Add source location and error message *)
let type_function (env : env_t) (f : func) : t list * t =
  check_dup_params None f.params;
  let param_env =
    List.fold_right
      (fun (i, t) env -> { env with vars = Idmap.add i (source_typ_to_t t) env.vars })
      f.params
      env
  in
  let new_env = type_var_dec_list param_env f.locals in
  List.iter (type_stmt None new_env (source_typ_to_t f.ret)) f.body;
  (List.map (fun (_, t) -> source_typ_to_t t) f.params,
   source_typ_to_t f.ret)

let type_prog (p : prog) : unit =
  let env = type_var_dec_list { funs = Idmap.empty; vars = Idmap.empty } p.globals in
  ignore (
    List.fold_left
      (fun env f ->
         let (arg_t, ret_t) = type_function env f in
         { env with funs = Idmap.add f.fun_name (arg_t, ret_t) env.funs })
      env
      p.funcs)

let rec remove_loc_stmts (stmts : stmt list) : stmt list =
  List.map remove_loc_one stmts

and remove_loc_one s =
  match s with
  | DoWhile (s1, e, s) ->
    DoWhile (remove_loc_one s1, e, remove_loc_one s)
  | Ite (e, s1, s2) ->
    Ite (e, remove_loc_one s1, remove_loc_one s2)
  | Stmts s ->
    Stmts (remove_loc_stmts s)
  | Loc (s, _) -> remove_loc_one s
  | s -> s

let remove_loc (p : prog) : prog =
  (* TODO *)
  p
