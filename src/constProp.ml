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

(* Do constant propagation and folding. Later compiler phases assume that no
   operation has 2 constant arguments, so this needs to be guaranteed here. *)

open SourceAst
module T = Tokens

(* Return n, such that 2^n = i, or None if there is no such *)
let log2 (i : int64) : int option =
  (* Linear search for the least significant 1 in the binary represenatation.
     A binary search might be faster, but probably not worth the hassle. *)
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

(* Decides whether evaluating an expression might have a side-effect *)
let rec might_have_effect (e : exp) : bool =
  match e with
  | Ident (_, []) -> false
  | Ident (_, _) -> true (* Array bound check failure *)
  | Num _ | Bool _ -> false
  | Op (e1, op, e2) ->
    if op = T.Div then
      true (* Divide by zero error *)
    else
      might_have_effect e1 || might_have_effect e2
  | Uop (_, e) -> might_have_effect e
  | Array es -> List.exists might_have_effect es
  | Call _ -> true (* The called function could do something *)

(* Check whether two expressions are equal, and don't have effects. This lets
   some operators remove them. Note that we are just comparing the structure of
   the expressions, so we won't see that x + (y + z) is equal to (x + y) + z,
   etc. *)
let ok_remove_eq_exp (e1 : exp) (e2 : exp) : bool =
  e1 = e2 &&
  not (might_have_effect e1) &&
  not (might_have_effect e2)

let between_0_63 (i : int64) : bool =
  Int64.compare i (-1L) = 1 && Int64.compare i 64L = -1

(* Statically evaluate an expression according to the identifier values in env,
   don't try to follow constants in arrays. Operates in a single bottom-up pass.
   Doesn't do anything with associativity or commutativity, so things like
   (x + 1) + 2 don't get changed. *)
let rec fold_exp (env : exp Idmap.t) (e : exp) : exp =
  match e with
  | Ident (i, []) ->
    (try Idmap.find i env
     with Not_found -> e)
  | Ident (i, es) -> Ident (i, List.map (fold_exp env) es)
  | Num n -> Num n
  | Bool b -> Bool b
  | Op (e1, op, e2) ->
    let o1 = fold_exp env e1 in
    let o2 = fold_exp env e2 in
    (match (o1, op, o2) with
     (* Plus *)
     | (Num n1, T.Plus, Num n2) -> Num (Int64.add n1 n2)
     | (Num 0L, T.Plus, e) | (e, T.Plus, Num 0L) -> e

     (* Minus *)
     | (Num n1, T.Minus, Num n2) -> Num (Int64.sub n1 n2)
     | (e, T.Minus, Num 0L) -> e
     | (e1, T.Minus, e2) when ok_remove_eq_exp e1 e2 -> Num 0L

     (* Times *)
     | (Num n1, T.Times, Num n2) -> Num (Int64.mul n1 n2)
     | (e, T.Times, Num 1L) | (Num 1L, T.Times, e) -> e
     | (Num -1L, T.Times, e) | (e, T.Times, Num -1L) ->
        Op (Num 0L, T.Minus, e)
     (* Can't use or-patterns for the following two, according to compiler warning *)
     | (Num 0L, T.Times, e) when not (might_have_effect e) ->
         Num 0L
     | (e, T.Times, Num 0L) when not (might_have_effect e) ->
         Num 0L
     | (e, T.Times, Num n) | (Num n, T.Times, e) ->
       (match log2 (Int64.abs n) with
        | None -> Op (o1, op, o2)
        | Some log ->
          let shift_op = Op (e, T.Lshift, Num (Int64.of_int log)) in
          if n < 0L then
            Op (Num 0L, T.Minus, shift_op)
          else
            shift_op)

     (* Div *)
     | (Num n1, T.Div, Num n2) when n2 <> 0L -> Num (Int64.div n1 n2)
     | (Num _, T.Div, Num 0L) ->
       (* This phase needs to guarantee that no operation has two constant
          arguments. It doesn't matter what the numerator is, when the
          divisor is 0, so we can just use an arbitrary variable. *)
       Op (Ident (Temp ("CP", 0), []), T.Div, Num 0L)
     | (e, T.Div, Num 1L) -> e

     (* Less *)
     | (Num n1, T.Lt, Num n2) -> Bool (Int64.compare n1 n2 < 0)
     | (e1, T.Lt, e2) when ok_remove_eq_exp e1 e2 -> Bool false

     (* Greater *)
     | (Num n1, T.Gt, Num n2) -> Bool (Int64.compare n1 n2 > 0)
     | (e1, T.Gt, e2) when ok_remove_eq_exp e1 e2 -> Bool false

     (* Equal *)
     | (Num n1, T.Eq, Num n2) -> Bool (Int64.compare n1 n2 = 0)
     | (e1, T.Eq, e2) when ok_remove_eq_exp e1 e2 -> Bool true

     (* Shift left *)
     | (Num n1, T.Lshift, Num n2) ->
       (* Ocaml's shift_left is only defined between 0 and 63 inclusive *)
       if between_0_63 n2 then
         Num (Int64.shift_left n1 (Int64.to_int n2))
       else
         Num 0L
     | (e, T.Lshift, Num 0L) -> e
     | (Num 0L, T.Lshift, e) when not (might_have_effect e) ->
       Num 0L

     (* Bitwise or *)
     | (Num n1, T.BitOr, Num n2) -> Num (Int64.logor n1 n2)
     | (Num 0L, T.BitOr, e) | (e, T.BitOr, Num 0L) -> e
     (* Can't use or-patterns for the following two, according to compiler warning *)
     | (Num 0xFFFFFFFFFFFFFFFFL, T.BitOr, e) when not (might_have_effect e) ->
       Num 0xFFFFFFFFFFFFFFFFL
     | (e, T.BitOr, Num 0xFFFFFFFFFFFFFFFFL) when not (might_have_effect e) ->
       Num 0xFFFFFFFFFFFFFFFFL

     (* Bitwise and *)
     | (Num n1, T.BitAnd, Num n2) -> Num (Int64.logand n1 n2)
     | (Num 0xFFFFFFFFFFFFFFFFL, T.BitAnd, e)
     | (e, T.BitAnd, Num 0xFFFFFFFFFFFFFFFFL) -> e
     (* Can't use or-patterns for the following two, according to compiler warning *)
     | (Num 0L, T.BitAnd, e) when not (might_have_effect e) -> Num 0L
     | (e, T.BitAnd, Num 0L) when not (might_have_effect e) -> Num 0L

     (* And *)
     | (Bool true, T.And, e) | (e, T.And, Bool true) -> e
     | (Bool false, T.And, _) -> Bool false
     | (e, T.And, Bool false) when not (might_have_effect e) -> Bool false
     | (e1, T.And, e2) when ok_remove_eq_exp e1 e2 -> e1

     (* Or *)
     | (Bool false, T.Or, e) | (e, T.Or, Bool false) -> e
     | (Bool true, T.Or, _) -> Bool true
     | (e, T.Or, Bool true) when not (might_have_effect e) -> Bool true
     | (e1, T.Or, e2) when ok_remove_eq_exp e1 e2 -> e1

     | _ -> Op (o1, op, o2))
  | Uop (uop, e) ->
    let o = fold_exp env e in
    (match (uop, o) with
     | (T.Not, Bool b) -> Bool (not b)
     | (T.Not, Uop (T.Not, e)) -> e
     | _ -> Uop (uop, o))
  | Array es ->
    Array (List.map (fold_exp env) es)
  | Call (f, es) ->
    Call (f, List.map (fold_exp env) es)

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
let merge_constants (_ : id) (v1 : exp option) (v2 : exp option)
  : exp option =
  match (v1,v2) with
  | (Some e1, Some e2) ->
    if same_const e1 e2 then
      Some e1
    else None
  | _ -> None

(* Do constant propagation. Accumulate an environment of definitely known
   constants at the end of stmts, given definitely known constants env at the
   start. *)
let rec prop_stmts (env : exp Idmap.t) (stmts : stmt list)
  : exp Idmap.t * stmt list =
  match stmts with
  | [] -> (env,[])
  | Assign (x, [], e) :: stmts ->
    let o1 = fold_exp env e in
    let first_env =
      if is_const o1 then Idmap.add x o1 env else Idmap.remove x env
    in
    let (env',stmts') = prop_stmts first_env stmts in
    (env', Assign (x, [], o1) :: stmts')
  | Assign (x, es, e) :: stmts ->
    let o = fold_exp env e in
    let os = List.map (fold_exp env) es in
    let (env', stmts') = prop_stmts env stmts in
    (env', Assign (x, os, o) :: stmts')
  | DoWhile (s0, e, s1) :: stmts ->
    (* s0 is always executed *)
    let (env_init, t) = prop_stmt env s0 in
    let o = fold_exp env_init e in
    (match o with
     | Bool false ->
       let (env', stmts') = prop_stmts env_init stmts in
       (env', t::stmts')
     | _ ->
       (* From this point, the loop might execute s1;s0 0 or more times, so we
          calculate env1 which contains the constants that are constant no
          matter how many times the loop is executed *)
       let env1 = prop_loop_body env_init [s1; s0] in
       (* Now re-calculate the constant propagation for the things constant no
          matter how many times we iterate the loop *)
       let (_, t1) = prop_stmt env1 s1 in
       let o = fold_exp env1 e in
       let (_, t0) = prop_stmt env1 s0 in
       let (env',stmts') = prop_stmts env1 stmts in
       (env', DoWhile (t0, o, t1) :: stmts'))
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
       let (env',stmts') =
         prop_stmts (Idmap.merge merge_constants env1 env2) stmts
       in
       (env', Ite (o1, os1, os2) :: stmts'))
  | Stmts (stmts1) :: stmts ->
    let (env1, os1) = prop_stmts env stmts1 in
    let (env', stmts') = prop_stmts env1 stmts in
    (env', Stmts (os1) :: stmts')
  | In x :: stmts ->
    let (env',stmts') = prop_stmts (Idmap.remove x env) stmts in
    (env', In x :: stmts')
  | Out x :: stmts ->
    let (env',stmts') = prop_stmts env stmts in
    (env', Out x :: stmts')
  | Loc (s, ln) :: stmts ->
    let (env1, o1) = prop_stmt env s in
    let (env', stmts') = prop_stmts env1 stmts in
    (env', Loc (o1, ln) :: stmts')
  | Return x :: _ ->
    (* We can't carry on past a return *)
    (env, [Return x])

and prop_stmt env (stmt : stmt) =
  match prop_stmts env [stmt] with
  | (env,[os]) -> (env,os)
  | _ -> assert false

(* Given possibly known constants env at the start, compute the definitely
   known constants at the end, assuming that stmts is run in a loop body an
   unknown number of times. *)
and prop_loop_body (env : exp Idmap.t) (stmts : stmt list) : exp Idmap.t =
  let (env', _) = prop_stmts env stmts in
  (* The next approximation of constants at the start *)
  let env'' = Idmap.merge merge_constants env env' in
  if Idmap.equal same_const env env'' then
    (* Same as last time, fixed point reached *)
    env''
  else
    prop_loop_body env'' stmts
