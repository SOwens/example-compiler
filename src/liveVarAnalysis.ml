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
exception Todo

type cfg_annot = { gen : Varset.t; kill : Varset.t; live_exit : Varset.t }
  [@@deriving show]

type cfg = (cfg_entry * cfg_annot) list
  [@@deriving show]

(* Add the assigned identifier in a to the generation set *)
let add_gen (a : atomic_exp) (gen : Varset.t) : Varset.t =
  match a with
  | Num _ -> gen
  | Ident i -> Varset.add i gen

(* Compute the gen and kill sets for a basic block, live_exit is set to empty *)
let analyse_block (b : basic_block) : cfg_annot =
  let rec analyse_block gen kill b =
    match b with
    | [] -> (gen, kill)
    | AssignOp (i, a1, op, a2) :: b ->
      analyse_block (add_gen a1 (add_gen a2 (Varset.remove i gen)))
        (Varset.add i kill)
        b
    | AssignAtom (i, a) :: b ->
      analyse_block (add_gen a (Varset.remove i gen)) (Varset.add i kill) b
    | Ld (i, addr) :: b ->
      analyse_block (Varset.remove i gen) (Varset.add i kill) b
    | St (i, addr) :: b ->
      analyse_block (Varset.add i gen) kill b
    | In i :: b ->
      analyse_block (Varset.remove i gen) (Varset.add i kill) b
    | Out i :: b ->
      analyse_block (Varset.add i gen) kill b
    | Alloc _ :: b -> raise Todo
  in
  let (gen,kill) = analyse_block Varset.empty Varset.empty (List.rev b) in
  { gen = gen; kill = kill; live_exit = Varset.empty }

(* Split the annotated cfg into the predecessors of node n, and the other nodes
*)
let rec find_preds n cfg =
  List.partition
    (fun (entry, annots) ->
       match entry.next with
       | End -> false
       | Next n' ->
         n = n'
       | Branch (i, n1, n2) ->
         n = n1 || n = n2)
    cfg

(* Do live variable analysis, returning an annotated cfg *)
let lva (cfg : BlockStructure.cfg) : cfg =
  (* Initial annotations for all of the blocks *)
  let init_worklist =
    List.map (fun entry -> (entry, analyse_block entry.elems)) cfg
  in
  (* The worklist and finished_list partition the whole cfg. That is, they must
     contain all of the blocks between them, with no duplication *)
  let rec do_one (worklist :cfg) (finished_list :cfg) : cfg =
    match worklist with
    | [] -> finished_list
    | ((entry, annot) as node) :: worklist ->
      let live_entry =
        Varset.union annot.gen (Varset.diff annot.live_exit annot.kill)
      in
      let (updates, worklist) = find_preds entry.bnum worklist in
      let (possible_updates, finished) =
        find_preds entry.bnum (node::finished_list)
      in
      let (finished', updates') =
        List.partition
          (fun (entry, annot) -> Varset.subset live_entry annot.live_exit)
          possible_updates
      in
      let new_worklist =
        List.map
          (fun (entry, annot) ->
             (entry, { annot with
                       live_exit = Varset.union annot.live_exit live_entry}))
          (updates @ updates')
        @
        worklist
      in
      do_one new_worklist (finished' @ finished)
  in
  do_one (List.rev init_worklist) []

(* Check that the operator cannot have a side-effect *)
let pure_op op : bool =
  match op with
  | Tokens.Div -> false (* divide by zero *)
  | _ -> true

let rec local_remove_unused_writes (live : Varset.t) (elems : block_elem list)
  : block_elem list =
  match elems with
  | [] -> []
  | AssignOp (i, a1, op, a2) :: b ->
    if Varset.mem i live && pure_op op then
      AssignOp (i, a1, op, a2) ::
      local_remove_unused_writes
        (add_gen a1 (add_gen a2 (Varset.remove i live))) b
    else
      local_remove_unused_writes live b
  | AssignAtom (i, a) :: b ->
    if Varset.mem i live then
      AssignAtom (i, a) ::
      local_remove_unused_writes (add_gen a (Varset.remove i live)) b
    else
      local_remove_unused_writes live b
  | Ld (i, addr) :: b ->
    if Varset.mem i live then
      Ld (i, addr) ::
      local_remove_unused_writes (Varset.remove i live) b
    else
      local_remove_unused_writes live b
  | St (i, addr) :: b ->
    St (i, addr) :: local_remove_unused_writes (Varset.add i live) b
  | In i :: b ->
    if Varset.mem i live then
      In i :: local_remove_unused_writes (Varset.remove i live) b
    else
      local_remove_unused_writes live b
  | Out i :: b ->
    Out i :: local_remove_unused_writes (Varset.add i live) b
  | Alloc _ :: b -> raise Todo

let add_test_vars (ae1, op, ae2) vars =
  let vars' =
    match ae1 with
    | Ident v -> Varset.add v vars
    | _ -> vars
  in
  match ae2 with
    | Ident v -> Varset.add v vars'
    | _ -> vars'

let add_exit_var (nb : next_block) (vars : Varset.t) : Varset.t =
  match nb with
  | End | Next _ -> vars
  | Branch (t, _, _) -> add_test_vars t vars

let remove_unused_writes (cfg : cfg) : cfg =
  List.map
    (fun (entry, annot) ->
       let new_elems =
         local_remove_unused_writes
           (add_exit_var entry.next annot.live_exit) (List.rev entry.elems)
       in
       ({entry with elems = List.rev new_elems}, annot))
    cfg
