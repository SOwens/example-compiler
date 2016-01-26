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

(* Do constant propagation and folding *)

open Util
open SourceAst

let log2 (i : int64) : int option =
  (* Linear search for the least significant 1 in the binary represenatation.
     A binary search would be faster, but maybe not worth the hassle? *)
  let rec f (i : int64) (shifted : int) : int option =
    if Int64.logand i 0x1L = 0L then
      f (Int64.shift_right i 1) (shifted + 1)
    else if i = 0x1L then
      Some shifted
    else
      None
  in
  if i > 0L then
    f i 0
  else
    None

(* Statically evaluate an expression according to the identifier values in env,
   don't try to follow constants in arrays.
   We don't do anything with associativity or commutativity, so things like
   (x + 1) + 2 don't get changed. It would be nice to add that. *)
let rec fold_exp (env : exp Strmap.t) (exp : exp) : exp =
  match exp with
  | Ident (i, []) ->
    (try Strmap.find i env
     with Not_found -> exp)
  | Ident (i, es) ->
    Ident (i, List.map (fold_exp env) es)
  | Num n -> Num n
  | Bool b -> Bool b
  | Oper (e1, op, e2) ->
    let o1 = fold_exp env e1 in
    let o2 = fold_exp env e2 in
    (match (o1, op, o2) with
     | (Num n1, T.Plus, Num n2) -> Num (Int64.add n1 n2)
     | (Num 0L, T.Plus, e) | (e, T.Plus, Num 0L) -> e

     | (Num n1, T.Minus, Num n2) -> Num (Int64.sub n1 n2)
     | (e, T.Minus, Num 0L) -> e
     | (Ident (i1, []), T.Minus, Ident (i2, [])) when i1 = i2 ->
         Num 0L

     | (Num n1, T.Times, Num n2) -> Num (Int64.mul n1 n2)
     | (e, T.Times, Num 1L) | (Num 1L, T.Times, e) -> e
     | (Num -1L, T.Times, e) | (e, T.Times, Num -1L) -> Oper (Num 0L, T.Minus, e)
     | (e, T.Times, Num 0L) | (Num 0L, T.Times, e) -> Num 0L
     | (e, T.Times, Num n) | (Num n, T.Times, e) ->
       (match log2 n with
       | None -> exp
       | Some log ->
         Oper (e, T.Lshift, Num (Int64.of_int log)))

     | (Num n1, T.Div, Num n2) when n2 <> 0L -> Num (Int64.div n1 n2)
     | (e, T.Div, Num 1L) -> e

     | (Num n1, T.Lt, Num n2) -> Bool (Int64.compare n1 n2 < 0)
     | (Ident (i1, []), T.Lt, Ident (i2, [])) when i1 = i2 -> Bool false

     | (Num n1, T.Gt, Num n2) -> Bool (Int64.compare n1 n2 > 0)
     | (Ident (i1, []), T.Gt, Ident (i2, [])) when i1 = i2 -> Bool false

     | (Num n1, T.Eq, Num n2) -> Bool (Int64.compare n1 n2 = 0)
     | (Ident (i1, []), T.Eq, Ident (i2, [])) when i1 = i2 -> Bool true

     | (Num n1, T.Lshift, Num n2) -> Num (Int64.shift_left n1 (Int64.to_int n2))
     | (e, T.Lshift, Num 0L) -> e
     | (Num 0L, T.Lshift, e) -> Num 0L

     | (Num n1, T.BitOr, Num n2) -> Num (Int64.logor n1 n2)
     | (Num 0L, T.BitOr, e) | (e, T.BitOr, Num 0L) -> e
     | (Num 0xFFFFFFFFFFFFFFFFL, T.BitOr, e) | (e, T.BitOr, Num 0xFFFFFFFFFFFFFFFFL) -> Num 0xFFFFFFFFFFFFFFFFL

     | (Num n1, T.BitAnd, Num n2) -> Num (Int64.logand n1 n2)
     | (Num 0L, T.BitAnd, e) | (e, T.BitAnd, Num 0L) -> Num 0L
     | (Num 0xFFFFFFFFFFFFFFFFL, T.BitAnd, e) | (e, T.BitAnd, Num 0xFFFFFFFFFFFFFFFFL) -> e

     | _ -> Oper (o1, op, o2))
  | Array es ->
    Array (List.map (fold_exp env) es)

let is_const (e : exp) : bool =
  match e with
  | Num _ | Bool _ -> true
  | _ -> false

let same_const (e1 : exp) (e2 : exp) : bool =
  match (e1,e2) with
  | (Num n1, Num n2) -> Int64.compare n1 n2 = 0
  | (Bool b1, Bool b2) -> b1 = b2
  | _ -> false

(* If v1 and v2 contain the same constant, return it. Otherwise return None *)
let merge_constants (id : id) (v1 : exp option) (v2 : exp option) : exp option =
  match (v1,v2) with
  | (Some e1, Some e2) ->
    if same_const e1 e2 then
      Some e1
    else None
  | _ -> None

(* Do constant propagation. Accumulate an environment of definitely known
   constants at the end of stmts, given definitely known constants env at the
   start. *)
let rec prop_stmts (env : exp Strmap.t) (stmts : stmt list) : exp Strmap.t * stmt list =
  match stmts with
  | [] -> (env,[])
  | In x :: stmts ->
    let (env',stmts') = prop_stmts (Strmap.remove x env) stmts in
    (env', In x :: stmts')
  | Out x :: stmts ->
    let (env',stmts') = prop_stmts env stmts in
    (env', Out x :: stmts')
  | Assign (x, [], e) :: stmts ->
    let o1 = fold_exp env e in
    let first_env =
      if is_const o1 then Strmap.add x o1 env else Strmap.remove x env
    in
    let (env',stmts') = prop_stmts first_env stmts in
    (env', Assign (x, [], o1) :: stmts')
  | Assign (x, es, e) :: stmts ->
    let o = fold_exp env e in
    let os = List.map (fold_exp env) es in
    let (env', stmts') = prop_stmts env stmts in
    (env', Assign (x, os, o) :: stmts')
  | DoWhile (s0, e, s1) :: stmts ->
    let (env', t1) = prop_stmts env [s1] in
    let o1 = fold_exp env' e in
    (match o1 with
     | Bool false -> prop_stmts env stmts
     | _ ->
       let (env1,os1) = prop_loop_body env [s1; s0] in
       (* have to redo the condition, for the sound constants on entry *)
       let (env1', t1) = prop_stmts env1 [s1] in
       let o1 = fold_exp env1' e in
       let (env1'',stmts') = prop_stmts env1' stmts in
       (env1'', DoWhile (o1, os1) :: stmts'))
  | Ite (e, s1, s2) :: stmts ->
    let o1 = fold_exp env e in
    (match o1 with
     | Bool true ->
       let (env1, os1) = prop_stmt env s1 in
       let (env', stmts') = prop_stmts env1 stmts in
       (env', os1::stmts')
     | Bool false ->
       let (env2, os2) = prop_stmt env s2 in
       let (env', stmts') = prop_stmts env2 stmts in
       (env', os2::stmts')
     | _ ->
       let (env1, os1) = prop_stmt env s1 in
       let (env2, os2) = prop_stmt env s2 in
       (* Only include constants that are known to be the same at the end of
          both branches of the Ite for the subsequent statements. *)
       let (env',stmts') = prop_stmts (Strmap.merge merge_constants env1 env2) stmts in
       (env', Ite (o1, os1, os2) :: stmts'))
  | Stmts (stmts1) :: stmts ->
    let (env1, os1) = prop_stmts env stmts1 in
    let (env', stmts') = prop_stmts env1 stmts in
    (env', Stmts (os1) :: stmts')
  | Loc _ :: _ -> raise (InternalError "Loc in constProp")

and prop_stmt env stmt =
  match prop_stmts env [stmt] with
  | (env,[os]) -> (env,os)
  | _ -> raise (InternalError "prop_stmt")

(* Given possibly known constants env at the start, compute the definitely
   known constants at the end, assuming that stmt is run in a loop body an
   unknown number of times. *)
and prop_loop_body (env : exp Strmap.t) stmt : exp Strmap.t * stmt =
  let (env', stmt') = prop_stmt env stmt in
  (* The next approximation of constants at the start *)
  let env'' = Strmap.merge merge_constants env env' in
  if Strmap.equal same_const env env'' then
    (* Same as last time, fixed point reached *)
    (env',stmt')
  else
    (* Use the original statement body, because stmt' was computed with too
       many constants *)
    prop_loop_body env'' stmt
