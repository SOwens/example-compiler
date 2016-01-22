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

(* Types *)
type t =
  | Tint
  | Tbool
  | Tarray of int (* The number of dimensions that the array has *)

(* Map identifiers to their types *)
type env_t = t Strmap.t

(* Raise a type error *)
let type_error (ln : int) (msg : string) : 'a =
  raise (BadInput ("Line " ^ string_of_int ln ^ ": " ^ msg))

(* Compute the type of an expression, or raise BadInput if there is a type error *)
let rec type_exp (env : env_t) (e : exp) : t =
  match e with
  | Ident (i, es, ln) ->
    let t =
      try Strmap.find i env
      with Not_found ->
        type_error ln ("Uninitialised variable " ^ i)
    in
    let ts = List.map (type_exp env) es in
    let l = List.length ts in
    (match t with
     | Tarray num_dims ->
       if num_dims = l && List.for_all (fun x -> x = Tint) ts then
         Tint
       else if num_dims = l then
         type_error ln "Array index with non-integer type"
       else
         type_error ln ("Array reference with " ^ string_of_int l ^ " indices, expected " ^
                          string_of_int num_dims)
     | t ->
       if l = 0 then
         t
       else
         type_error ln ("Attempt to index non-array variable " ^ i))
  | Num n -> Tint
  | Bool b -> Tbool
  | Oper (e1, op, e2) ->
    let t1 = type_exp env e1 in
    let t2 = type_exp env e2 in
    (match (t1, op, t2) with
     | (Tbool, ((T.And | T.Or), _), Tbool) -> Tbool
     | (Tint, ((T.Plus | T.Minus | T.Times | T.Div | T.Lshift | T.BitOr | T.BitAnd), _), Tint) -> Tint
     | (Tint, ((T.Lt | T.Eq | T.Gt), _), Tint) -> Tbool
     | (_, (op, ln), _) ->
       type_error ln ("Type error on operator " ^ T.show_op op))
  | Array (es, ln) ->
    let ts = List.map (type_exp env) es in
    let l = List.length ts in
    if l = 0 then
      type_error ln "Array must have at least 1 dimension"
    else if List.for_all (fun x -> x = Tint) ts then
      Tarray l
    else
      type_error ln "Array dimension with non-integer type"

(* Type check an identifier being assigned to.
   If it is already assigned, check its type. If it has not been assigned,
   extend the type environment *)
let type_lhs_ident (env :env_t) (x : id) (t : t) (ln : int) : env_t =
  try
    if Strmap.find x env = t then
      env
    else
      type_error ln ("Bad type for " ^ x)
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
      type_error ln "Output with non-integer type"
  | Assign (x, [], e, ln) :: stmts' ->
    let t = type_exp env e in
    let env' = type_lhs_ident env x t ln in
    type_stmts env' stmts'
  | Assign (x, es, e, ln) :: stmts' ->
    (* Assignments to arrays require the lhs to be checkable as an expression.
       In particular, the identifier must already be bound to an array type of
       the correct dimension. *)
    let t1 = type_exp env (Ident (x, es, ln)) in
    let t2 = type_exp env e in
    if t1 = t2 then
      type_stmts env stmts'
    else
      type_error ln "Array assignment type mismatch"
  | While (e, s, ln) :: stmts ->
    if type_exp env e = Tbool then
      (type_stmts env [s];
       type_stmts env stmts)
    else
      type_error ln "While test of non-bool type"
  | Ite (e, s1, s2, ln) :: stmts ->
    if type_exp env e = Tbool then
      (type_stmts env [s1];
       type_stmts env [s2];
       type_stmts env stmts)
    else
      type_error ln "If test of non-bool type"
  | Stmts (s_list, ln) :: stmts' ->
    type_stmts env s_list;
    type_stmts env stmts'
