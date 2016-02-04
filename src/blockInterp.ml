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

(* An interpreter for CFGs *)

open BlockStructure
open AstInterp

type store_t = int64 Varmap.t

type heap_t = { mutable next_address : int; heap : bytes }

(* Convert int64 to 8 bytes in little-endian order and place them in the
   given bytes at the given index *)
let int64_to_bytes (b: bytes) index num : unit =
  let rec loop i inc =
    if inc < 8 then
      (Bytes.set b (index + inc) (Char.chr (Int64.to_int (Int64.logand 0xFFL i)));
       loop (Int64.shift_right i 8) (inc + 1))
    else
      ()
  in
  loop num 0

(* Read 8 bytes in little-endian order into an int64 *)
let bytes_to_int64 (b : bytes) index : int64 =
  let rec loop i inc =
    if inc >= 0 then
      let next =
        Int64.logor
          (Int64.shift_left i 8)
          (Int64.of_int (Char.code (Bytes.get b (index + inc))))
      in
      loop next (inc - 1)
    else
      i
  in
  loop 0L 7

let alloc1 (heap : heap_t) (n : int64) : int64 =
  let old_next = heap.next_address in
  let new_next = old_next + 8 * ((Int64.to_int n) + 1) in
  if new_next <= Bytes.length heap.heap then
    (heap.next_address <- new_next;
     int64_to_bytes heap.heap old_next n;
     Int64.of_int old_next)
  else
    raise (Crash "Out of memory")

let rec alloc (heap : heap_t) (ns : int64 list) : int64 =
  match ns with
  | [] -> raise (Util.InternalError "Empty alloc")
  | [n] -> alloc1 heap n
  | n::ns ->
    let v = alloc1 heap n in
    for i = 0 to (Int64.to_int n) do
      int64_to_bytes heap.heap (Int64.to_int v + 8 * i)
        (alloc heap ns)
    done;
    v

let interp_atomic_exp (store : store_t) (ae : atomic_exp) : int64 =
  match ae with
  | Ident x -> Varmap.find x store
  | Num i -> i

let interp_block_elem (store : store_t) (heap : heap_t) (be : block_elem)
  : store_t =
  match be with
  | AssignOp (x, ae1, op, ae2) ->
    let n1 = interp_atomic_exp store ae1 in
    let n2 = interp_atomic_exp store ae2 in
    Varmap.add x (do_op op n1 n2) store
  | AssignAtom (x, ae) ->
    Varmap.add x (interp_atomic_exp store ae) store
  | Ld (x1, x2, ae) ->
    let n1 = Varmap.find x2 store in
    let n2 = interp_atomic_exp store ae in
    Varmap.add
      x1
      (bytes_to_int64 heap.heap (Int64.to_int (Int64.add n1 n2)))
      store
  | St (x, ae1, ae2) ->
    let n1 = Varmap.find x store in
    let n2 = interp_atomic_exp store ae1 in
    let n3 = interp_atomic_exp store ae2 in
    int64_to_bytes heap.heap (Int64.to_int (Int64.add n1 n2)) n3;
    store
  | Call (Some x, "input", []) ->
    Printf.printf "> ";
    (try
       let n = Int64.of_string (read_line ()) in
       Varmap.add x n store
     with Failure _ -> raise (Crash "not a 64-bit integer"))
  | Call (None, "output", [i]) ->
    begin
      print_string (Int64.to_string (interp_atomic_exp store i));
      print_newline ();
      store
    end
  | Call (Some x,
          ("allocate1" | "allocate2" | "allocate3" | "allocate4" | "allocate5" |
           "allocate6" | "allocate7"),
          aes) ->
    let ns = List.map (interp_atomic_exp store) aes in
    Varmap.add x (alloc heap ns) store
  | Call _ ->
    raise (Util.InternalError "Unknown function call in blockInterp")

let rec interp_basic_block (store : store_t) (heap : heap_t)
    (bb : basic_block) : store_t =
  match bb with
  | [] -> store
  | be::bes ->
    let store' = interp_block_elem store heap be in
    interp_basic_block store' heap bes

let interp_next_block store nb : int =
  match nb with
  | End -> 0
  | Next i -> i
  | Branch ((ae1, op, ae2), i1, i2) ->
    let n1 = interp_atomic_exp store ae1 in
    let n2 = interp_atomic_exp store ae2 in
    match op with
    | Eq -> if Int64.compare n1 n2 = 0 then i1 else i2
    | Lt -> if Int64.compare n1 n2 < 0 then i1 else i2
    | Gt -> if Int64.compare n1 n2 > 0 then i1 else i2

let interp_cfg  (store : store_t) (heap : heap_t) (cfg : cfg) : unit =
  let cfgmap = LineariseCfg.init_traversal cfg in
  let rec loop (index : int) store =
    let node = Util.Intmap.find index cfgmap in
    let store' = interp_basic_block store heap node.elems in
    let next_index = interp_next_block store node.next in
    loop next_index store'
  in
  loop 1 store

let interp_prog (heap_size : int) (cfg : cfg) : unit =
  interp_cfg
    Varmap.empty
    { next_address = 0; heap = Bytes.make heap_size '\000'}
    cfg
