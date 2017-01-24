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
open BlockStructure

(* Copy and paste from extlib to get input_file without making a dependency.
   extlib is LGPL 2.1, and so this sub-module is too.
   https://github.com/ygrek/ocaml-extlib/blob/33f744ddb28d6a0f4c96832145e1a6e384644709/src/extList.ml
*)

exception Invalid_index of int

type 'a mut_list =  {
  hd: 'a;
  mutable tl: 'a list
}
external inj : 'a mut_list -> 'a list = "%identity"
let split_nth index = function
  | [] -> if index = 0 then [],[] else raise (Invalid_index index)
  | (h :: t as l) ->
    if index = 0 then [],l
    else if index < 0 then raise (Invalid_index index)
    else
      let rec loop n dst l =
        if n = 0 then l else
        match l with
        | [] -> raise (Invalid_index index)
        | h :: t ->
          let r = { hd =  h; tl = [] } in
          dst.tl <- inj r;
          loop (n-1) r t
      in
      let r = { hd = h; tl = [] } in
      inj r, loop (index-1) r t
(* end copy/paste *)

(* The basic type signature of a monad *)
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Add an operation for getting the number of a variable, and for running the
   computation *)
module type VarNumMonad = sig
  include Monad
  val inc_var : var -> unit t
  val get_counts : int Varmap.t t
  val run : 'a t -> 'a
end

(* Implement the monad *)
module M : VarNumMonad = struct
  (* A monadic computation will be a function from the current map and next
     number to use to a new map, new next number, and result value *)
  type 'a t = int Varmap.t -> int Varmap.t * 'a

  (* return is defined as a standard state monad *)
  let return x map = (map, x)

  (* bind is defined as a standard state monad *)
  let bind x f map =
    let (new_map, res) = x map in
    f res new_map

  let inc_var v map =
    try
      let i = Varmap.find v map in
      (Varmap.add v (i + 1) map, ())
    with
    | Not_found ->
      (Varmap.add v 1 map, ())

  let get_counts map = (map, map)

  let run f =
    let (_, x) = f Varmap.empty in
    x
end

let (>>=) = M.bind

let mcons p q = p >>= (fun x -> q >>= (fun y -> M.return (x::y)))

let sequence (l : 'a M.t list) : 'a list M.t =
  List.fold_right mcons l (M.return [])

let mapM (f : 'a -> 'b M.t) (al : 'a list) : 'b list M.t =
  sequence (List.map f al)

let sequence_ (l : unit M.t list) : unit M.t =
  let mcons p q = p >>= (fun x -> q >>= (fun y -> M.return ())) in
  List.fold_right mcons l (M.return ())

let mapM_ (f : 'a -> unit M.t) (al : 'a list) : unit M.t =
  sequence_ (List.map f al)

let count_vars_ae (ae : atomic_exp) : unit M.t =
  match ae with
  | Ident r ->
    M.inc_var r >>= (fun _ -> M.return ())
  | Num x -> M.return ()

let count_vars_be (be : block_elem) : unit M.t =
  match be with
  | AssignOp (r, ae1, op, ae2) ->
    M.inc_var r >>= (fun _ ->
    count_vars_ae ae1 >>= (fun _ ->
    count_vars_ae ae2 >>= (fun _ ->
    M.return ())))
  | AssignAtom (r, ae) ->
    M.inc_var r >>= (fun _ ->
    count_vars_ae ae >>= (fun _ ->
    M.return ()))
  | Ld (v1, v2, ae) ->
    M.inc_var v1 >>= (fun _ ->
    M.inc_var v2 >>= (fun _ ->
    count_vars_ae ae >>= (fun _ ->
    M.return ())))
  | St (r, ae1, ae2) ->
    M.inc_var r >>= (fun _ ->
    count_vars_ae ae1 >>= (fun _ ->
    count_vars_ae ae2 >>= (fun _ ->
    M.return ())))
  | Call (None, f, aes) ->
    mapM_ count_vars_ae aes >>= (fun _ ->
    M.return ())
  | Call (Some i, f, aes) ->
    M.inc_var i >>= (fun _ ->
    mapM_ count_vars_ae aes >>= (fun _ ->
    M.return ()))
  | BoundCheck (a1, a2) ->
    count_vars_ae a1 >>= (fun _ ->
    count_vars_ae a2 >>= (fun _ ->
    M.return ()))

let count_vars_test (ae1, op, ae2) : unit M.t =
  let vars =
    match (ae1, ae2) with
    | (Ident v1, Ident v2) -> [v1;v2]
    | (Ident v1, Num _) -> [v1]
    | (Num _, Ident v1) -> [v1]
    | (Num _, Num _) -> []
  in
  mapM_ M.inc_var vars

let count_vars_nb (nb : next_block) : unit M.t =
  match nb with
  | End -> M.return ()
  | Next i -> M.return ()
  | Branch (r, t1, t2) ->
    count_vars_test r >>= (fun _ ->
    M.return ())

let count_vars (cfg : cfg) : int Varmap.t =
  let m =
    mapM_
      (fun e ->
         mapM_ count_vars_be e.elems >>= (fun _ ->
         count_vars_nb e.next >>= (fun _ ->
         M.return ())))
      cfg >>= (fun _ -> M.get_counts >>= M.return)
  in
  M.run m

let reg_alloc_ae (map : var Varmap.t) (ae : atomic_exp) : atomic_exp =
  match ae with
  | Ident v ->
    Ident (Varmap.find v map)
  | Num x -> Num x

let reg_alloc_be (map : var Varmap.t) (be : block_elem) : block_elem =
  match be with
  | AssignOp (v, ae1, op, ae2) ->
    AssignOp (Varmap.find v map, reg_alloc_ae map ae1, op, reg_alloc_ae map ae2)
  | AssignAtom (v, ae) ->
    AssignAtom (Varmap.find v map, reg_alloc_ae map ae)
  | Ld (v1, v2, ae) ->
    Ld (Varmap.find v1 map, Varmap.find v2 map, reg_alloc_ae map ae)
  | St (v, ae1, ae2) ->
    St (Varmap.find v map, reg_alloc_ae map ae1, reg_alloc_ae map ae2)
  | Call (v, f, aes) ->
    Call (Util.option_map (fun v -> Varmap.find v map) v, f,
          List.map (reg_alloc_ae map) aes)
  | BoundCheck (a1, a2) ->
    BoundCheck (reg_alloc_ae map a1, reg_alloc_ae map a2)


let reg_alloc_test (map : var Varmap.t) (ae1, op, ae2) : test =
  (reg_alloc_ae map ae1, op, reg_alloc_ae map ae2)

let reg_alloc_nb (map : var Varmap.t) (nb : next_block) : next_block =
  match nb with
  | End -> End
  | Next i -> Next i
  | Branch (t, t1, t2) ->
    Branch (reg_alloc_test map t, t1, t2)

(* The returned int is the number of variables put on the stack *)
let reg_alloc (num_regs : int) (cfg : cfg) : (cfg * int) =
  let counts = count_vars cfg in
  let counts_list = Varmap.bindings counts in
  let sorted_counts_list =
    List.map fst (List.sort (fun (_, x) (_, y) -> compare y x) counts_list)
  in
  let num_regs = min num_regs (List.length sorted_counts_list) in
  let num_stacks = List.length sorted_counts_list - num_regs in
  let (in_regs,on_stack) = split_nth num_regs sorted_counts_list in
  let reg_nums = List.map (fun x -> Vreg x) (count num_regs) in
  let stack_nums = List.map (fun x -> Stack x) (count num_stacks) in
  let alloc_list = zip in_regs reg_nums @ zip on_stack stack_nums in
  let map = List.fold_right (fun (k,v) m -> Varmap.add k v m) alloc_list Varmap.empty in
  let cfg =
    List.map (fun entry -> { bnum = entry.bnum;
                             elems = List.map (reg_alloc_be map) entry.elems;
                             next = reg_alloc_nb map entry.next;
                             finished = false;
                             started = false }) cfg in
  (cfg, num_stacks)
