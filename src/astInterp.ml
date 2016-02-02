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

(* An interpreter for ASTs *)

open Util
open SourceAst
module T = Tokens

(* For when the interpreter crashed, such as array bounds violations *)
exception Crash of string

(* For errors that a well-typed program can't have *)
exception TypeError

(* Values are either integers or n-dimensional arrays of integers.
   We keep multi-dimensional arrays in a single dimensional one and include a
   list of how big each dimension is.
   We represent bools as numbers: true = 1L and false = 0L *)
type val_t =
  | Vint of int64
  | Varray of int list * int64 array

let val_t_to_int (v : val_t) : int64 =
  match v with
  | Vint i -> i
  | Varray _ -> raise TypeError

let val_t_to_array (v : val_t) : int list * int64 array =
  match v with
  | Vint _ -> raise TypeError
  | Varray (dims, a) -> (dims, a)

(* Given the array's dimensions, work out the slot for a particular set of
   indices. sizes and indices must have the same length.  Return None if one
   of the indices is out of bounds, i.e. greater than the size. *)
let array_address (sizes : int list) (indices : int list) : int option =
  (* acc keeps track of the product of the dimensions seen so far *)
  let rec f sizes indices (acc : int) =
    match (sizes, indices) with
    | ([], []) -> 0
    | (s1::sizes, i1::indices) ->
        i1 * acc + f sizes indices (s1 * acc)
    | _ ->
      raise TypeError
  in
  if List.for_all2 (fun s i -> i < s) sizes indices then
    Some (f sizes indices 1)
  else
    None

(* The store will map variable names to values *)
type store_t = val_t Idmap.t

let bool_to_int64 (b : bool) : int64 =
  if b then 1L else 0L

let int64_to_bool (i : int64) : bool =
  if Int64.compare i 0L = 0 then false else true

(* Do a primitive operation *)
let do_op op (n1 : int64) (n2 : int64) : int64 =
  match op with
  | T.Plus -> Int64.add n1 n2
  | T.Minus -> Int64.sub n1 n2
  | T.Times -> Int64.mul n1 n2
  | T.Div ->
    if n2 = 0L then
      raise (Crash "Division by 0")
    else
      Int64.div n1 n2
  | T.Lt -> bool_to_int64 (Int64.compare n1 n2 < 0)
  | T.Gt -> bool_to_int64 (Int64.compare n1 n2 > 0)
  | T.Eq -> bool_to_int64 (Int64.compare n1 n2 = 0)
  | T.Lshift ->
    let i = Int64.to_int n2 in
    (* Int64.shift_left is wacky outside of this interval *)
    if 0 <= i && i < 64 then
      Int64.shift_left n1 i
    else
      0L
  | T.BitOr -> Int64.logor n1 n2
  | T.BitAnd -> Int64.logand n1 n2
  | T.And | T.Or -> assert false

let do_uop uop (n : int64) : int64 =
  match uop with
  | T.Not ->
    bool_to_int64 (not (int64_to_bool n))

(* Compute the value of an expression *)
let rec interp_exp (store : store_t) (e : exp) : val_t =
  match e with
  | Ident (i, []) ->
    Idmap.find i store (* i will be in the store in a well-typed program *)
  | Ident (i, iexps) ->
    let (sizes, a) = val_t_to_array (Idmap.find i store) in
    let indices =
      List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp store e))))
        iexps
    in
    (match array_address sizes indices with
     | Some x -> Vint (Array.get a x)
     | None -> raise (Crash "array index out of bounds"))
  | Num n -> Vint n
  | Bool b -> Vint (bool_to_int64 b)
  | Op (e1, T.And, e2) ->
    (match interp_exp store e1 with
     | Vint n ->
       if int64_to_bool n then
         interp_exp store e2
       else
         Vint n
     | _ -> raise TypeError)
   | Op (e1, T.Or, e2) ->
    (match interp_exp store e1 with
     | Vint n ->
       if int64_to_bool n then
         Vint n
       else
         interp_exp store e2
     | _ -> raise TypeError)
  | Op (e1, op, e2) ->
    (match (interp_exp store e1, interp_exp store e2) with
     | (Vint n1, Vint n2) -> Vint (do_op op n1 n2)
     | _ -> raise TypeError)
  | Uop (uop, e) ->
    (match interp_exp store e with
     | Vint n -> Vint (do_uop uop n)
     | _ -> raise TypeError)
  | Array iexps ->
    let indices =
      List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp store e))))
        iexps
    in
    Varray (indices,
            Array.make (List.fold_right (fun x y -> x * y) indices 1) 0L)

(* Run a statement *)
let rec interp_stmt (store : store_t) (s : stmt) : store_t =
  match s with
  | Assign (i, [], e) ->
    let v = interp_exp store e in
    Idmap.add i v store
  | Assign (i, iexps, e) ->
    (match Idmap.find i store with
     | Varray (sizes, a) ->
       (let indices =
          List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp store e))))
            iexps
        in
        match array_address sizes indices with
        | None -> raise (Crash "array index out of bounds")
        | Some x ->
          let v = val_t_to_int (interp_exp store e) in
          Array.set a x v;
          store)
     | Vint _ -> raise TypeError)
  | DoWhile (head_s, e, body_s) ->
    let s1 = interp_stmt store head_s in
    if not (int64_to_bool (val_t_to_int (interp_exp s1 e))) then
      s1
    else
      let s2 = interp_stmt s1 body_s in
      interp_stmt s2 s
  | Ite (e, s1, s2) ->
    if int64_to_bool (val_t_to_int (interp_exp store e)) then
      interp_stmt store s1
    else
      interp_stmt store s2
  | Stmts sl ->
    interp_stmts store sl
  | In i ->
    Printf.printf "> ";
    (try
       let n = Int64.of_string (read_line ()) in
       Idmap.add i (Vint n) store
     with Failure _ -> raise (Crash "not a 64-bit integer"))
  | Out i ->
    begin
      print_string (Int64.to_string (val_t_to_int (Idmap.find i store)));
      print_newline ();
      store
    end
  | Loc _ ->
    raise (InternalError "Location annotation in interp")

and interp_stmts (store : store_t) (sl : stmt list) : store_t = match sl with
  | [] -> store
  | s::sl ->
    let store2 = interp_stmt store s in
    interp_stmts store2 sl;;
