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

(* A very simple register allocator. Global variables never get put in a
   register. Function parameters stay in the registers that they are passed in.
   Local variables get put in the remaining registers in no particular order.
   The rest are put on the stack *)

open Util
open BlockStructure

let get_vars_ae (ae : atomic_exp) (vars : Varset.t) : Varset.t =
  match ae with
  | Ident r -> Varset.add r vars
  | Num _ -> vars

let get_vars_be (be : block_elem) (vars : Varset.t) : Varset.t =
  match be with
  | AssignOp (r, ae1, _, ae2) ->
    get_vars_ae ae1 (get_vars_ae ae2 (Varset.add r vars))
  | AssignAtom (r, ae) -> get_vars_ae ae (Varset.add r vars)
  | Ld (v1, v2, ae) ->
    get_vars_ae ae (Varset.add v1 (Varset.add v2 vars))
  | St (r, ae1, ae2) ->
    get_vars_ae ae1 (get_vars_ae ae2 (Varset.add r vars))
  | Call (None, _, aes) ->
    List.fold_right get_vars_ae aes vars
  | Call (Some i, _, aes) ->
    List.fold_right get_vars_ae aes (Varset.add i vars)
  | BoundCheck (a1, a2) ->
    get_vars_ae a1 (get_vars_ae a2 vars)
  | NullCheck v ->
    Varset.add v vars

let get_vars_test (ae1, _, ae2) (vars : Varset.t) : Varset.t =
  List.fold_right get_vars_ae [ae1; ae2] vars

let get_vars_nb (nb : next_block) (vars : Varset.t) : Varset.t =
  match nb with
  | Return None -> vars
  | Return (Some v) -> Varset.add v vars
  | Next _ -> vars
  | Branch (r, _, _) -> get_vars_test r vars

let get_vars_block (b : cfg_entry) (vars : Varset.t) : Varset.t =
  List.fold_right get_vars_be b.elems (get_vars_nb b.next vars)

(* Get all of the variables mentioned in the cfg *)
let get_vars (cfg : cfg) : Varset.t =
  List.fold_right get_vars_block cfg Varset.empty

let is_global v =
  match v with
  | NamedSource (_, SourceAst.Global) -> true
  | _ -> false

let is_param v =
  match v with
  | NamedSource (_, SourceAst.Parameter) -> true
  | _ -> false

let is_local v =
  match v with
  | NamedSource (_, SourceAst.Local) -> true
  | NamedTmp _ -> true
  | _ -> false

(* Given the vars used in a function, allocate them into registers. Ignore the
   live ranges and clash graph. This is the simplest strategy that will yield a
   working program. Return also the number of stack slots used. *)
(* TODO: actually put some locals into registers *)
let build_regalloc_map (fun_params : (SourceAst.id * int) list) (vars : Varset.t)
  : (var Varmap.t * int) =
  let vars = Varset.elements vars in
  let (globals, vars) = List.partition is_global vars in
  let (locals, vars) = List.partition is_local vars in
  assert (List.for_all is_param vars); (* There shouldn't be any allocated registers already *)
  let global_map =
    List.map
      (fun g ->
         match g with
         | NamedSource (i, _) as n -> (n, Global i)
         | _ -> assert false)
      globals
  in
  let param_map =
    List.map (fun (p, i) -> (id_to_var p, Vreg i)) fun_params
  in
  let local_map =
    List.map2 (fun l i -> (l, Stack i)) locals (count (List.length locals))
  in
  let m =
    List.fold_right (fun (k,v) m -> Varmap.add k v m)
      (global_map @ param_map @ local_map)
      Varmap.empty
  in
  (m, List.length locals)

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
  | NullCheck v ->
    NullCheck (Varmap.find v map)

let reg_alloc_test (map : var Varmap.t) (ae1, op, ae2) : test =
  (reg_alloc_ae map ae1, op, reg_alloc_ae map ae2)

let reg_alloc_nb (map : var Varmap.t) (nb : next_block) : next_block =
  match nb with
  | Return (Some v) -> Return (Some (Varmap.find v map))
  | Return None -> Return None
  | Next i -> Next i
  | Branch (t, t1, t2) ->
    Branch (reg_alloc_test map t, t1, t2)

(* fun_params is an association list mapping the function's parameters to the
   registers that they were passed in. num_regs is the total number of
   registers that can be used. The returned int is the number of variables
   put on the stack *)
let reg_alloc (fun_params : (SourceAst.id * int) list) (num_regs : int)
    (cfg : cfg)
  : (cfg * int) =
  let (map, num_stacks) = build_regalloc_map fun_params (get_vars cfg) in
  let cfg =
    List.map (fun entry -> { bnum = entry.bnum;
                             elems = List.map (reg_alloc_be map) entry.elems;
                             next = reg_alloc_nb map entry.next;
                             finished = false;
                             started = false }) cfg in
  (cfg, num_stacks)
