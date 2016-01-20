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

open SourceAst
module T = Tokens

(* Get rid of &&, ||, and = expressions, rely on the fact that they have to be
   at the top level of an expressions (due to the type system), so we don't
   have to worry about 1 + (a && b) etc. Necessary to support short-circuit
   evaluation. 

   x := e1 && e2 --> x := e1; if x then x := e2 else {} 
   x := e1 || e2 --> x := e1; if x then {} else x := e2

*)
let rec remove_and_or_exp (id : id) (loc : int) (e : exp) : stmt list = 
  match e with
  | Bool true -> [Assign (id, Num 1L, loc)]
  | Bool false -> [Assign (id, Num 0L, loc)]
  | Oper (e1, (T.And, loc2), e2) ->
    remove_and_or_exp id loc e1 @
    [Ite (Ident (id,loc), Stmts (remove_and_or_exp id loc e2, loc), Stmts ([], loc), loc)]
  | Oper (e1, (T.Or, loc2), e2) ->
    remove_and_or_exp id loc e1 @
    [Ite (Ident (id,loc), Stmts ([], loc), Stmts (remove_and_or_exp id loc e2, loc), loc)]
  | _ -> [Assign (id, e, loc)]

let is_and_or_exp (e : exp) : bool =
  match e with
  | Oper (_, ((T.And | T.Or), _), _) -> true
  | _ -> false

(* Assume that the program can't use __tmp as a variable, and the the compiler
   won't use it elsewhere 

   if e is an && or || expression, do the following, else expand __tmp = e

   while e s -> __tmp := e; while __tmp {s; __tmp = e}
   if e then s1 else s2 --> __tmp := e; if __tmp then s1 else s2

*)
let rec remove_and_or_stmt (s : stmt) : stmt list =
  match s with
  | Assign (id, e, loc) -> 
    remove_and_or_exp id loc e
  | While (e, s, loc) when is_and_or_exp e ->
    let tmp_eq_e = remove_and_or_exp "__tmp" loc e in
    tmp_eq_e @
    [While (Ident ("__tmp", loc), Stmts (s :: tmp_eq_e, loc), loc)]
  | Ite (e, s1, s2, loc) when is_and_or_exp e ->
    remove_and_or_exp "__tmp" loc e @ [Ite (Ident ("__tmp", loc), s1, s2, loc)]
  | Stmts (sl, loc) ->
    List.flatten (List.map remove_and_or_stmt sl)
  | s -> [s]

let rec remove_and_or (s : stmt list) : stmt list =
  List.flatten (List.map remove_and_or_stmt s)
