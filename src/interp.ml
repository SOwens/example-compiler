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

open Util
open SourceAst

exception Crash of string

(* Values are either integers or n-dimensional arrays of integers.
   We keep multi-dimensional arrays in a single dimensional one and include a
   list of how big each dimension is. *)
type val_t =
  | Vint of Int64.t
  | Varray of int list * Int64.t array

(* Given the array's dimensions, work out the slot for a particular set of
   indices. Return None if one of the indices is out of bounds, i.e. greater
   than the size. *)
let array_address (sizes : int list) (indices : int list) : int option =
  let rec f sizes indices acc =
    match (sizes, indices) with
    | ([], []) -> 0
    | (s1::sizes, i1::indices) ->
        i1 * acc + f sizes indices (acc * s1)
    | _ -> assert false
  in
  assert (List.length sizes = List.length indices);
  if List.for_all2 (fun s i -> i < s) sizes indices then
    Some (f sizes indices 1)
  else
    None

type store_t = val_t Strmap.t

let bool_to_int64 (b : bool) : Int64.t = 
  if b then 1L else 0L
let int64_to_bool (i : Int64.t) : bool = 
  if Int64.compare i 0L = 0 then false else true

(* Do a primitive operation *)
let do_op op (n1 : Int64.t) (n2 : Int64.t) : Int64.t = match op with
  | T.Plus -> Int64.add n1 n2
  | T.Minus -> Int64.sub n1 n2
  | T.Times -> Int64.mul n1 n2
  | T.Div -> Int64.div n1 n2
  | T.Lt -> bool_to_int64 (Int64.compare n1 n2 = -1)
  | T.Gt -> bool_to_int64 (Int64.compare n1 n2 = 1)
  | T.Eq -> bool_to_int64 (Int64.compare n1 n2 = 0)
  | T.And -> bool_to_int64 (int64_to_bool n1 && int64_to_bool n2)
  | T.Or -> bool_to_int64 (int64_to_bool n1 || int64_to_bool n2)
  | T.Lshift -> Int64.shift_left n1 (Int64.to_int n2)
  | T.BitOr -> Int64.logor n1 n2
  | T.BitAnd -> Int64.logand n1 n2

(* Compute the value of an expression *)
let rec interp_exp (store : store_t) (e : exp) : val_t =
  match e with
  | Ident (i, [], _) -> Strmap.find i store
  | Ident (i, indices, _) ->

  | Num n -> Vint n
  | Bool b -> Vint (bool_to_int64 b)
  | Oper (e1, (op, _), e2) ->
    (match (interp_exp store e1, interp_exp store e2) with
     | (Vint n1, Vint n2) ->
       Vint (do_op op n1 n2)
     | _ ->
       raise (Crash "operator given non-integer value"))

(* Run a statement *)
let rec interp_stmt (store : store_t) (s : stmt) : store_t = match s with
  | Assign (i, e, _) ->
    let n = interp_exp store e in
    Strmap.add i n store
  | While (e, body_s, loc) ->
    if not (int64_to_bool (interp_exp store e)) then
      store
    else
      let store2 = interp_stmt store body_s in
      interp_stmt store2 s
  | Ite (e, s1, s2, _) ->
    if int64_to_bool (interp_exp store e) then
      interp_stmt store s1
    else
      interp_stmt store s2
  | Stmts (sl, _) ->
    interp_stmts store sl
  | In (i, _) ->
    Printf.printf "> ";
    (try
       let n = Int64.of_string (read_line ()) in
       Strmap.add i n store
     with Failure _ -> raise (BadInput "not a 64-bit integer"))
  | Out (i, _) ->
    begin
      print_string (Int64.to_string (Strmap.find i store));
      print_newline ();
      store
    end
and interp_stmts (store : store_t) (sl : stmt list) : store_t = match sl with
  | [] -> store
  | s::sl ->
    let store2 = interp_stmt store s in
    interp_stmts store2 sl;;

let filename = ref None;;

let usage_msg = 
  "example interpreter \nexample usage:       interp.byte test.expl\n";; 
    
let _ = 
  Arg.parse []
    (fun s -> 
       match !filename with
       | None ->
         filename := Some s
       | Some s' ->
         (Format.printf "Error: given multiple files to run: %s and %s\n" s' s;
          exit 1))
    usage_msg;;

let _ = 
  match !filename with
  | None ->
    (print_string usage_msg;
     exit 1)
  | Some filename ->
    interp_stmts Strmap.empty (FrontEnd.front_end filename false)
