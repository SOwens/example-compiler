(*
 * Example compiler
 * Copyright (C) 2015-2017 Scott Owens
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
   | Ident of SourceAst.id * ae
   | Op of ae * op * ae
   | Uop of ae
   | Array of ae list
   | Call of SourceAst.id * ae list
 *)

open SourceAst
module T = Tokens

let is_atomic (e : exp) : bool =
  match e with
  | Num _ -> true
  | Bool _ -> true
  | Ident (_, []) -> true
  | Ident (_, _) -> false
  | Op _ -> false
  | Uop _ -> false
  | Array _ -> false
  | Call _ -> false

let is_flat (e : exp) : bool =
  match e with
  | Num _ -> true
  | Bool _ -> true
  | Ident (_, []) -> true
  | Ident (_, [e]) -> is_atomic e
  | Ident (_, _) -> false
  | Op (e1, _, e2) -> is_atomic e1 && is_atomic e2
  | Uop (_, e) -> is_atomic e
  | Array es -> List.for_all is_atomic es
  | Call (_, es) -> List.for_all is_atomic es

let unnest (stmts : stmt list) : stmt list =

  (* Generate unique names for temporary variables *)
  let next_ident = ref 0 in
  let get_ident () : id =
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    Temp ("UE", x)
  in

  (* indices must all be atomic, according to is_atomic above. Returns a flat
     expression according to is_flat. *)
  let rec unnest_indices (arr : id) (indices : exp list) : stmt list * exp =
    match indices with
    | [] -> ([], Ident (arr, []))
    | [i] -> ([], Ident (arr, [i]))
    | (i::indices) ->
      let id = get_ident () in
      let (s, e) = unnest_indices id indices in
      (Assign (id, [], Ident (arr, [i])) :: s,
       e)
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
    | Ident (i, es) ->
      let (s_list, aes) = List.split (List.map unnest_exp_atomic es) in
      let (s_list2, ae) = unnest_indices i aes in
      (List.flatten s_list @ s_list2, ae)
    | Op (e1, T.And, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      (* f1 && f2 --> (if f1 then tmp := f2 else t := false); tmp *)
      let tmp = get_ident () in
      (s1 @
       [Ite (f1,
             Stmts (s2 @ [Assign (tmp, [], f2)]),
             Assign (tmp, [], Bool false))],
       (Ident (tmp, [])))
    | Op (e1, T.Or, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      (* f1 || f2 --> (if f1 then tmp := true else tmp := f2 ); tmp *)
      let tmp = get_ident () in
      (s1 @
       [Ite (f1,
             Assign (tmp, [], Bool true),
             Stmts (s2 @ [Assign (tmp, [], f2)]))],
       (Ident (tmp, [])))
    | Op (e1, op, e2) ->
      let (s1, a1) = unnest_exp_atomic e1 in
      let (s2, a2) = unnest_exp_atomic e2 in
      (s1 @ s2, Op (a1, op, a2))
    | Uop (uop, e) ->
      let (s, a) = unnest_exp_atomic e in
      (s, Uop (uop, a))
    | Array es ->
      let (s_list, aes) = List.split (List.map unnest_exp_atomic es) in
      (List.flatten s_list, Array aes)
    | Call (f, es) ->
      let (s_list, aes) = List.split (List.map unnest_exp_atomic es) in
      (List.flatten s_list, Call  (f, aes))


(* Similar to unnest_exp, but ensures that the returned exp is atomic, rather
   than just flat.  *)
  and unnest_exp_atomic (e : exp) : stmt list * exp =
    let (s, f) = unnest_exp e in
    if is_atomic f then
      (s, f)
    else
      let id = get_ident () in
      (s @ [Assign (id, [], f)], Ident (id, []))

  and unnest_exp_for_test (e : exp) : stmt list * exp =
    let (s, f) = unnest_exp e in
    match f with
    | Ident (_, [_]) ->
      let id = get_ident () in
      (s @ [Assign (id, [], f)], Ident (id, []))
    | _ -> (s, f)
  in

  let rec unnest_stmt (s : stmt) : stmt list =
    match s with
    | Assign (x, [], e) ->
      let (s, f) = unnest_exp e in
      s @ [Assign (x, [], f)]
    | Assign (x, es, e) ->
      let (s_list, aes) = List.split (List.map unnest_exp_atomic es) in
      let (s, f) = unnest_exp_atomic e in
      (match unnest_indices x aes with
       | (s', Ident (i, [])) ->
         List.flatten s_list @ s' @ s @ [Assign (i, [], f)]
       | (s', Ident (i, [f'])) ->
         List.flatten s_list @ s' @ s @ [Assign (i, [f'], f)]
       | _ -> assert false)
    | DoWhile (s0, e, s1) ->
      let s0' = unnest_stmt s0 in
      let (se1, e') = unnest_exp_for_test e in
      let s1' = unnest_stmt s1 in
      [DoWhile (stmts_to_stmt (s0' @ se1), e', stmts_to_stmt s1')]
    | Ite (e, s1, s2) ->
      let (se, e') = unnest_exp_for_test e in
      let s1' = unnest_stmt s1 in
      let s2' = unnest_stmt s2 in
      se @ [Ite (e', stmts_to_stmt s1', stmts_to_stmt s2')]
    | Stmts s_list ->
      List.flatten (List.map unnest_stmt s_list)
    | In id -> [In id]
    | Out id -> [Out id]
    | Loc (stmt, _) ->
      unnest_stmt stmt
    | Return id -> [Return id]
  in

  List.flatten (List.map unnest_stmt stmts)
