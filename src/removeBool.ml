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

(* Convert booleans to integers and remove all && and || expressions with
   proper short circuit evaluation *)

open SourceAst
module T = Tokens

(* Get rid of booleans, &&, and || on the right of an assignment. Rely on
   the fact that they have to be at the top level of an expression (due to the
   type system), so we don't have to worry about 1 + (a && b) etc. Necessary to
   support short-circuit evaluation.

   x := e1 && e2 --> x := e1; if x then x := e2 else {}
   x := e1 || e2 --> x := e1; if x then {} else x := e2
*)
let rec remove_and_or_exp (id : id) (e : exp) : stmt list =
  match e with
  | Bool true -> [Assign (id, [], Num 1L)]
  | Bool false -> [Assign (id, [], Num 0L)]
  | Oper (e1, T.And, e2) ->
    remove_and_or_exp id e1 @
    [Ite (Ident (id, []), Stmts (remove_and_or_exp id e2), Stmts [])]
  | Oper (e1, T.Or, e2) ->
    remove_and_or_exp id e1 @
    [Ite (Ident (id, []), Stmts [], Stmts (remove_and_or_exp id e2))]
  | _ -> [Assign (id, [], e)]

let is_and_or_exp (e : exp) : bool =
  match e with
  | Oper (_, (T.And | T.Or), _) -> true
  | _ -> false

(* Assume that the program can't use __tmp as a variable, and the the compiler
   won't use it elsewhere

   if e is an && or || expression, do the following, else expand __tmp = e

   while e s -> __tmp := e; while __tmp {s; __tmp = e}
   if e then s1 else s2 --> __tmp := e; if __tmp then s1 else s2

*)
let rec remove_and_or_stmt (s : stmt) : stmt list =
  match s with
  | Assign (id, [], e) ->
    remove_and_or_exp id e
  | While (e, s) ->
    let sl = remove_and_or_stmt s in
    if is_and_or_exp e then
      let tmp_eq_e = remove_and_or_exp "__tmp" e in
      tmp_eq_e @
      [While (Ident ("__tmp", []), Stmts (sl @ tmp_eq_e))]
    else
      (match sl with
       | [s] -> [While (e, s)]
       | _ -> [While (e, Stmts sl)])
  | Ite (e, s1, s2) ->
    let sl1 = remove_and_or_stmt s1 in
    let sl2 = remove_and_or_stmt s2 in
    let s1' =
      match sl1 with
      | [x] -> x
      | _ -> Stmts (sl1)
    in
    let s2' =
      match sl2 with
      | [x] -> x
      | _ -> Stmts (sl2)
    in
    if is_and_or_exp e then
      remove_and_or_exp "__tmp" e @ [Ite (Ident ("__tmp", []), s1', s2')]
    else
      [Ite (e, s1', s2')]
  | Stmts sl ->
    List.flatten (List.map remove_and_or_stmt sl)
  | In _ | Out _ | Assign _ ->
    (* Cannot contain any && or || expressions because of the type system *)
    [s]
  | Loc _ ->
    raise (Util.InternalError "Loc in removeBool")

and remove_and_or (sl : stmt list) : stmt list =
  List.flatten (List.map remove_and_or_stmt sl)
