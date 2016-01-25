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

(* A control flow graph representation with basic blocks, and the source AST ->
   CGF algorithm *)

open Util
module S = SourceAst

exception Todo

type var =
  | Vreg of int
  | Stack of int
  | NamedSource of string
  | NamedTmp of int
  [@@deriving ord]

let show_var v =
  match v with
  | Vreg i -> "r" ^ string_of_int i
  | Stack i -> "s" ^ string_of_int i
  | NamedSource s -> s
  | NamedTmp i -> "tmp" ^ string_of_int i

let pp_var fmt v =
  Format.fprintf fmt "%s" (show_var v)

module VarCmp = struct
  type t = var
  let compare = compare_var
end

module Varset' = Set.Make(VarCmp)

module Varset = struct
  include Varset'
  let show s = [%show: var list] (elements s)
  let pp fmt s = Format.fprintf fmt "%a" (pp_set pp_var) (elements s)
end

module Varmap = Map.Make(VarCmp)

(* Atomic expressions *)
type atomic_exp =
  | Ident of var
  | Num of int64

let show_atomic_exp ae =
  match ae with
  | Ident v -> show_var v
  | Num i -> [%show: int64] i

let pp_atomic_exp fmt ae =
  Format.fprintf fmt "%s" (show_atomic_exp ae)

(* A single entry in a basic block *)
type block_elem =
  | AssignOp of var * atomic_exp * Tokens.op * atomic_exp
  | AssignAtom of var * atomic_exp
  | Ld of var * atomic_exp
  | St of var * atomic_exp
  | In of var
  | Out of var
  [@@deriving show]

let pp_block_elem fmt be =
  match be with
  | AssignOp (v, ae1, op, ae2) ->
    Format.fprintf fmt "%a := %a %s %a"
      pp_var v
      pp_atomic_exp ae1
      (Tokens.op_to_string op)
      pp_atomic_exp ae2
  | AssignAtom (v, ae) ->
    Format.fprintf fmt "%a := %a"
      pp_var v
      pp_atomic_exp ae
  | Ld (v, ae) ->
    Format.fprintf fmt "%a := *%a"
      pp_var v
      pp_atomic_exp ae
  | St (v, ae) ->
    Format.fprintf fmt "*%a := %a"
      pp_var v
      pp_atomic_exp ae
  | In v ->
    Format.fprintf fmt "input %a"
      pp_var v
  | Out v ->
    Format.fprintf fmt "output %a"
      pp_var v

type basic_block = block_elem list
  [@@deriving show]

(* A basic block is either at the end of the program, or there is an
   unconditional jump out of it, or a branch out of it. This type represents
   the block number of the next block in the cfg. *)
type next_block =
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of var * int * int
  [@@deriving show]

let pp_next_block fmt nb =
  match nb with
  | End -> Format.fprintf fmt "END"
  | Next i -> Format.fprintf fmt "B%d" i
  | Branch (v,i1,i2) -> Format.fprintf fmt "if@ %a@ then@ B%d@ else@ B%d"
                          pp_var v
                          i1
                          i2

(* An adjacency list representation for the CFG *)
type cfg_entry =
  { bnum : int; elems : block_elem list; next : next_block;
    mutable started : bool; mutable finished : bool }
  [@@deriving show]

let pp_cfg_entry fmt e =
  Format.fprintf fmt "@[[B%d:@ %a@ %a]@]"
    e.bnum
    (pp_list pp_block_elem) e.elems
    pp_next_block e.next

type cfg = cfg_entry list
  [@@deriving show]



(* Build the control-flow graph *)
let build_cfg (stmts : S.stmt list) : cfg =
  (* A counter to get new indices for blocks *)
  let next_block_num = ref 0 in
  let get_block_num () =
    let x = !next_block_num in
    next_block_num := 1 + !next_block_num;
    x
  in

  (* Store the cfg here as we build it *)
  let the_cfg = ref [] in
  let add_block (num : int) (block : basic_block) (next : next_block) : unit =
    the_cfg := { bnum = num; elems = List.rev block; next = next;
                 started = false; finished = false} :: !the_cfg
  in

  (* Generate unique names for temporary variables *)
  let next_ident = ref 1 in
  let get_ident () : var =
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    NamedTmp x
  in

  (* Convert an expression to an atomic expression, along with a list of basic
     block entries that perform the computation of the expression. Essentially,
     this flattens out the expression by using a temporary to store the results
     of each sub-expression *)
  let rec exp_to_atomic (e : S.exp) : atomic_exp * basic_block =
    match e with
    | S.Ident (i, []) -> (Ident (NamedSource i), [])
    | S.Ident (i, es) -> raise Todo
    | S.Num i -> (Num i, [])
    | S.Bool _ -> raise (InternalError "Bool in blockStructure")
    | S.Oper (e1, op, e2) ->
      let (a1, s1) = exp_to_atomic e1 in
      let (a2, s2) = exp_to_atomic e2 in
      let ident = get_ident () in
      (Ident ident, AssignOp (ident, a1, op, a2) :: s2 @ s1)
    | S.Array [] ->
      raise (InternalError "0-dim array in blockStructure")
    | S.Array es ->
      raise Todo
  in

  (* Like exp_to_atomic, but strip the Identifier off of the expression,
     but fail if the atomic is a num or bool, which can only happen if the
     source exp is a num or bool. *)
  let exp_to_atomic_test (e : S.exp) : var * basic_block =
    match exp_to_atomic e with
    | (Ident i, stmts) -> (i, stmts)
    | (Num _, _) -> raise (InternalError "exp_to_atomic")
  in

  (* Convert stmts to basic blocks, and add them to the_cfg. block_num is the
     index for the first block, ret_block for the block to return to after
     stmts. block_acc accumulates the block that we've seen so far. *)
  let rec find_blocks (block_num : int) (ret_block : int) (stmts : S.stmt list) (block_acc : basic_block) : unit =
    match stmts with
    | [] ->
      add_block block_num block_acc (Next ret_block)
    | S.In x :: s ->
      find_blocks block_num ret_block s (In (NamedSource x) :: block_acc)
    | S.Out x :: s ->
      find_blocks block_num ret_block s (Out (NamedSource x) :: block_acc)
    | S.Assign (x, [], e) :: s1 ->
      let (a, s2) = exp_to_atomic e in
      find_blocks block_num ret_block s1
        (AssignAtom (NamedSource x, a) :: s2 @ block_acc)
    | S.Assign (x, es, e) :: s1 ->
      raise Todo
    | S.Stmts s1 :: s2 ->
      find_blocks block_num ret_block (s1 @ s2) block_acc
    | S.DoWhile (s_head, e, s1) :: s2 ->
      let (i, s3) = exp_to_atomic_test e in
      let header_block_n = get_block_num () in
      let body_block_n = get_block_num () in
      add_block block_num block_acc (Next header_block_n);
      find_blocks body_block_n header_block_n [s1] [];
      if s2 = [] then
        add_block header_block_n s3
          (Branch (i, body_block_n, ret_block))
      else
        let following_block_n = get_block_num () in
        add_block header_block_n s3
          (Branch (i, body_block_n, following_block_n));
        find_blocks following_block_n ret_block s2 []
    | S.Ite (e, s1, s2) :: s3 ->
      let (i, s4) = exp_to_atomic_test e in
      let true_block_n = get_block_num () in
      let false_block_n = get_block_num () in
      add_block block_num (s4 @ block_acc)
        (Branch (i, true_block_n, false_block_n));
      if s3 = [] then
        (find_blocks true_block_n ret_block [s1] [];
         find_blocks false_block_n ret_block [s2] [])
      else
        let following_block_n = get_block_num () in
        find_blocks true_block_n following_block_n [s1] [];
        find_blocks false_block_n following_block_n [s2] [];
        find_blocks following_block_n ret_block s3 []
    | ((S.Loc _) :: _) ->
      raise (InternalError "Loc in blockStructure")
  in

  let end_block_num = get_block_num () in
  let init_block = get_block_num () in (* Later on we rely on the starting block being #1 *)
  add_block end_block_num [] End;
  find_blocks init_block end_block_num stmts [];
  !the_cfg
