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

(* Flatten expressions so that they follow this grammar. We don't introduce a
   new type, but do define predicates on the SourceAst.exp type.

   type ae =
   | Num of int64
   | Bool of bool
   | Ident of SourceAst.id

   type flat_exp =
   | Num of int64
   | Bool of bool
   | Ident of SourceAst.id * ae list
   | Op of ae * op * ae
   | Uop of ae
   | Array of ae list
 *)

open Util
open SourceAst
module T = Tokens

exception Todo

let is_atomic (e : exp) : bool =
  match e with
  | Num _ -> true
  | Bool _ -> true
  | Ident (_, []) -> true
  | Ident (_, es) -> false
  | Op _ -> false
  | Uop _ -> false
  | Array _ -> false

let is_flat (e : exp) : bool =
  match e with
  | Num _ -> true
  | Bool _ -> true
  | Ident (_, []) -> true
  | Ident (_, es) -> List.for_all is_atomic es
  | Op (e1, op, e2) -> is_atomic e1 && is_atomic e2
  | Uop (uop, e) -> is_atomic e
  | Array es -> List.for_all is_atomic es

let rec unnest (stmts : stmt list) : stmt list =

  (* Generate unique names for temporary variables *)
  let next_ident = ref 1 in
  let get_ident () : id =
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    Temp x
  in

  (* Flatten out and expression into a list of statements and an expression by
     using temporary variables to store the results of each sub-expression. The
     expression returned satisfies is_flat above.

     Warning: As implemented, this is O(n^2) because of the list appending.
     This is unlikely to matter for human written code, as it is O(n^2) where n
     is the size of an expression, not the whole program. However, for
     compiling machine-generated code, this could be a problem. A little care
     could be taken to collect results in a tree-structure and then convert it
     into a list. *)
  let rec unnest_exp (e : exp) : stmt list * exp =
    match e with
    | Num i -> ([], Num i)
    | Bool b -> ([], Bool b)
    | Ident (i, []) -> ([], Ident (i, []))
    | Ident (i, es) -> raise Todo
    | Op (e1, T.And, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      (* f1 && f2 --> (if f1 then tmp := f2 else t := false); tmp *)
      let tmp = get_ident () in
      (s1 @ s2 @
       [Ite (f1, Assign (tmp, [], f2), Assign (tmp, [], Bool false))],
       (Ident (tmp, [])))
    | Op (e1, T.Or, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      (* f1 || f2 --> (if f1 then tmp := true else tmp := f2 ); tmp *)
      let tmp = get_ident () in
      (s1 @ s2 @
       [Ite (f1, Assign (tmp, [], Bool true), Assign (tmp, [], f2))],
       (Ident (tmp, [])))
    | Op (e1, op, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      if is_atomic f1 && is_atomic f2 then
        (s1 @ s2, Op (f1, op, f2))
      else if is_atomic f1 then
        let id2 = get_ident () in
        (s1 @ s2 @ [Assign (id2, [], f2)],
         Op (f1, op, Ident (id2, [])))
      else if is_atomic f2 then
        let id1 = get_ident () in
        (s1 @ s2 @ [Assign (id1, [], f1)],
         Op (Ident (id1, []), op, f2))
      else
        let id1 = get_ident () in
        let id2 = get_ident () in
        (s1 @ s2 @ [Assign (id1, [], f1); Assign (id2, [], f2)],
         Op (Ident (id1, []), op, Ident (id2, [])))
    | Uop (uop, e) ->
      let (s, f) = unnest_exp e in
      if is_atomic f then
        (s, Uop (uop, f))
      else
        let id = get_ident () in
        (s @ [Assign (id, [], f)], Uop (uop, f))
    | Array es ->
      raise Todo
  in

  let stmts_to_stmt (s : stmt list) : stmt =
    match s with
    | [s1] -> s1
    | _ -> Stmts s
  in

  let rec unnest_stmt (s : stmt) : stmt list =
    match s with
    | Assign (x, [], e) ->
      let (s', f) = unnest_exp e in
        s' @ [Assign (x, [], f)]
    | Assign (x, es, e) ->
      raise Todo
    | DoWhile (s0, e, s1) ->
      let s0' = unnest_stmt s0 in
      let (se1, e') = unnest_exp e in
      let s1' = unnest_stmt s1 in
      [DoWhile (stmts_to_stmt (s0' @ se1), e', stmts_to_stmt s1')]
    | Ite (e, s1, s2) ->
      let (se, e') = unnest_exp e in
      let s1' = unnest_stmt s1 in
      let s2' = unnest_stmt s2 in
      se @ [Ite (e', stmts_to_stmt s1', stmts_to_stmt s2')]
    | Stmts s_list ->
      List.flatten (List.map unnest_stmt s_list)
    | In id -> [In id]
    | Out id -> [Out id]
    | Loc (stmt, int) ->
      unnest_stmt stmt
  in

  List.flatten (List.map unnest_stmt stmts)
