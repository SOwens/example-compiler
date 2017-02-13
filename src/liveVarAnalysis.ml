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

open Util
open BlockStructure

type cfg_annot = { gen : Varset.t; kill : Varset.t; live_exit : Varset.t }

type cfg = (cfg_entry * cfg_annot) list

let pp_cfg_annot fmt c =
  Format.fprintf fmt "{@[gen=%a;@ kill=%a; live_exit=%a@]}"
    Varset.pp c.gen
    Varset.pp c.kill
    Varset.pp c.live_exit

let pp_cfg1 fmt (c,a) =
  Format.fprintf fmt "(@[%a@ %a@])"
    BlockStructure.pp_cfg_entry c
    pp_cfg_annot a

let pp_cfg fmt l = pp_list pp_cfg1 fmt l

(* Add the identifier in a to the generation set *)
let add_gen (a : atomic_exp) (gen : Varset.t) : Varset.t =
  match a with
  | Num _ -> gen
  | Ident i -> Varset.add i gen

(* Add the identifiers in aes to the generation set *)
let rec add_gen_list (aes : atomic_exp list) (gen : Varset.t) : Varset.t =
  match aes with
  | [] -> gen
  | a::aes -> add_gen_list aes (add_gen a gen)

(* Compute the gen and kill sets for a basic block, live_exit is set to empty *)
let analyse_block (b : basic_block) : cfg_annot =
  let rec analyse_block gen kill b =
    match b with
    | [] -> (gen, kill)
    | AssignOp (i, a1, _, a2) :: b ->
      analyse_block (add_gen a1 (add_gen a2 (Varset.remove i gen)))
        (Varset.add i kill)
        b
    | AssignAtom (i, a) :: b ->
      analyse_block (add_gen a (Varset.remove i gen)) (Varset.add i kill) b
    | Ld (i, j, a) :: b ->
      analyse_block (add_gen a (Varset.add j (Varset.remove i gen)))
        (Varset.add i kill)
        b
    | St (i, a1, a2) :: b ->
      analyse_block (add_gen a1 (add_gen a2 (Varset.add i gen)))
        kill
        b
    | Call (None, _, aes) :: b ->
      analyse_block (add_gen_list aes gen) kill b
    | Call (Some i, _, aes) :: b ->
      analyse_block (add_gen_list aes (Varset.remove i gen)) (Varset.add i kill) b
    | BoundCheck (a1, a2) :: b ->
      analyse_block (add_gen a1 (add_gen a2 gen)) kill b
    | NullCheck v :: b ->
      analyse_block (Varset.add v gen) kill b
  in
  let (gen,kill) = analyse_block Varset.empty Varset.empty (List.rev b) in
  { gen = gen; kill = kill; live_exit = Varset.empty }

(* Split the annotated cfg into the predecessors of node n, and the other nodes
*)
let find_preds n cfg =
  List.partition
    (fun (entry, _) ->
       match entry.next with
       | Return _ -> false
       | Next n' ->
         n = n'
       | Branch (_, n1, n2) ->
         n = n1 || n = n2)
    cfg

let add_test_vars (ae1, _, ae2) vars =
  let vars' =
    match ae1 with
    | Ident v -> Varset.add v vars
    | _ -> vars
  in
  match ae2 with
    | Ident v -> Varset.add v vars'
    | _ -> vars'

(* What's live right before we are exit a block: initially if the block
   returns, then all the globals are alive, since other parts of the code might
   read them. Also the returned value is live. For a branch, any variable used
   in the condition is live. *)
let init_live_exit (globals : Varset.t) (a : cfg_annot) (next : next_block)
  : cfg_annot =
  match next with
  | Return None -> { a with live_exit = globals }
  | Return (Some i) -> { a with live_exit = Varset.add i globals }
  | Next _ -> a
  | Branch (i, _, _) ->
    { a with live_exit = add_test_vars i Varset.empty }

(* Do live variable analysis, returning an annotated cfg *)
let lva (globals : Varset.t) (cfg : BlockStructure.cfg) : cfg =
  (* Initial annotations for all of the blocks. *)
  let init_worklist =
    List.map
      (fun entry ->
         (entry,
          init_live_exit globals (analyse_block entry.elems) entry.next))
      cfg
  in
  (* Update one block from the worklist.
    The worklist and finished_list partition the whole cfg. That is, they must
     contain all of the blocks between them, with no duplication. NB, this is
     slightly different than the naive worklist algorithm. Here we add a
     node to the worklist only when its live_exit would change based on the current
     node's live_entry. *)
  let rec do_one (worklist : cfg) (finished_list : cfg) : cfg =
    match worklist with
    | [] -> finished_list
    | ((entry, annot) as node) :: worklist ->
      let live_entry =
        Varset.union annot.gen (Varset.diff annot.live_exit annot.kill)
      in
      (* Updates contains node's predecessors from the worklist input,
         and worklist contains the remaining non-predecessors. *)
      let (updates, worklist) = find_preds entry.bnum worklist in
      (* Possible_updates contains node's predecessors from the finished_list,
         possibly including itself, and finished contains the remaining
         non-predecessors. *)
      let (possible_updates, finished) =
        find_preds entry.bnum (node::finished_list)
      in
      (* Finished' contains all possible_updates whose live_exits are supersets
         of our live entry. There is no need to update them, because no nodes would
         be added to their live_exit. updates' contains those previously finished nodes
         whose live_exits will change *)
      let (finished', updates') =
        List.partition
          (fun (_, annot) -> Varset.subset live_entry annot.live_exit)
          possible_updates
      in
      (* Update the live_exits of the nodes needing updated and add them to the
         worklist *)
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

(* Remove unused writes from elems, assuming that elems is presented backwards,
   and live is the set of variables live on exiting the block *)
let rec local_remove_unused_writes (live : Varset.t) (elems : block_elem list)
  : block_elem list =
  match elems with
  | [] -> []
  | AssignOp (i, a1, op, a2) :: b ->
    if Varset.mem i live || not (pure_op op) then
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
  | Ld (i, j, a) :: b ->
    if Varset.mem i live then
      Ld (i, j, a) ::
      local_remove_unused_writes (add_gen a (Varset.add j (Varset.remove i live))) b
    else
      local_remove_unused_writes live b
  | St (i, a1, a2) :: b ->
    St (i, a1, a2) ::
    local_remove_unused_writes (add_gen a1 (add_gen a2 (Varset.add i live))) b
  | Call (i, f, aes) :: b ->
    Call (i, f, aes) ::
    let live' =
      match i with
      | None -> live
      | Some v -> Varset.remove v live
    in
    local_remove_unused_writes (add_gen_list aes live') b
  | BoundCheck (a1, a2) :: b ->
    BoundCheck (a1, a2) :: local_remove_unused_writes (add_gen a1 (add_gen a2 live)) b
  | NullCheck v :: b ->
    NullCheck v :: local_remove_unused_writes (Varset.add v live) b

let remove_unused_writes (cfg : cfg) : cfg =
  List.map
    (fun (entry, annot) ->
       let new_elems =
         local_remove_unused_writes annot.live_exit (List.rev entry.elems)
       in
       ({entry with elems = List.rev new_elems}, annot))
    cfg
