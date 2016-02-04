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
   CGF algorithm. Also compiles arrays to loads and stores. *)

open Util
module S = SourceAst

type var =
  | Vreg of int
  | Stack of int
  | NamedSource of string
  | NamedTmp of string * int
  [@@deriving ord]

let show_var v =
  match v with
  | Vreg i -> "r" ^ string_of_int i
  | Stack i -> "s" ^ string_of_int i
  | NamedSource s -> s
  | NamedTmp (s, i) -> "_tmp_" ^ s ^ string_of_int i

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
  (* Ld (x,y,e) represents x := *(y+e) *)
  | Ld of var * var * atomic_exp
  (* St (x,e1,e2) represents *(x+e1) := e2 *)
  | St of var * atomic_exp * atomic_exp
  | Call of var option * string * atomic_exp list
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
  | Ld (v1, v2, ae) ->
    Format.fprintf fmt "%a := *(%a+%a)"
      pp_var v1
      pp_var v2
      pp_atomic_exp ae
  | St (v, ae1, ae2) ->
    Format.fprintf fmt "*(%a+%a) := %a"
      pp_var v
      pp_atomic_exp ae1
      pp_atomic_exp ae2
  | Call (Some v, x, aes) ->
    Format.fprintf fmt "%a := %s%a"
      pp_var v
      x
      (pp_list pp_atomic_exp) aes
  | Call (None, x, aes) ->
    Format.fprintf fmt "%s%a"
      x
      (pp_list pp_atomic_exp) aes

type basic_block = block_elem list
  [@@deriving show]

type test_op =
  | Lt
  | Gt
  | Eq
  [@@deriving show]

let pp_test_op fmt op =
  match op with
  | Lt -> Format.fprintf fmt "<"
  | Gt -> Format.fprintf fmt ">"
  | Eq -> Format.fprintf fmt "="

type test = atomic_exp * test_op * atomic_exp
  [@@deriving show]

let pp_test fmt (ae1, op, ae2) =
  Format.fprintf fmt "%a %a %a"
    pp_atomic_exp ae1
    pp_test_op op
    pp_atomic_exp ae2

(* A basic block is either at the end of the program, or there is an
   unconditional jump out of it, or a branch out of it. This type represents
   the block number of the next block in the cfg. *)
type next_block =
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of test * int * int
  [@@deriving show]

let pp_next_block fmt nb =
  match nb with
  | End -> Format.fprintf fmt "END"
  | Next i -> Format.fprintf fmt "B%d" i
  | Branch (t,i1,i2) -> Format.fprintf fmt "if@ %a@ then@ B%d@ else@ B%d"
                          pp_test t
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

let id_to_var (i : S.id) : var =
  match i with
  | S.Source s -> NamedSource s
  | S.Temp (s, i) -> NamedTmp (s, i)

let bool_to_num b =
  if b then Num 1L else Num 0L

let exp_to_atomic (e : S.exp) : atomic_exp =
  match e with
  | S.Ident (id, []) -> Ident (id_to_var id)
  | S.Num n -> Num n
  | S.Bool b -> bool_to_num b
  | S.Ident (_, _::_) | S.Op _ | S.Uop _ | S.Array _ ->
    raise (InternalError "non-flat expression in blockStructure")

let tmp_var = NamedTmp("BS", 0)

(* Convert x := e into block elements, returned in *reverse* order *)
let flat_e_to_assign (x : S.id) (e : S.exp) : block_elem list =
  let v = id_to_var x in
  match e with
  | S.Ident (id, []) ->
    [AssignAtom (v, Ident (id_to_var id))]
  | S.Ident (id, [ae]) ->
    let ae = exp_to_atomic ae in
    (* v := id[ae] --> tmp_var := *id;
                       assert ae < length_var;
                       v := *(id + (ae+1) * 8) *)
    let get_len = Ld (tmp_var, id_to_var id, Num 0L) in
    (* TODO assert ae < tmp_var *)
    (match ae with
     | Num n ->
       [Ld (v, id_to_var id, Num (Int64.shift_left (Int64.add n 1L) 3)); get_len]
     | _ ->
       [Ld (v, id_to_var id, Ident tmp_var);
        AssignOp (tmp_var, Ident tmp_var, Tokens.Lshift, Num 3L);
        AssignOp (tmp_var, ae, Tokens.Plus, Num 1L);
        get_len])
  | S.Ident (id, _::_::_) ->
    raise (InternalError "multi-dimension array index in blockStructure")
  | S.Num n -> [AssignAtom (v, Num n)]
  | S.Bool b -> [AssignAtom (v, bool_to_num b)]
  | S.Op (ae1, op, ae2) ->
    [AssignOp (v, exp_to_atomic ae1, op, exp_to_atomic ae2)]
  | S.Uop (Tokens.Not, ae) ->
    raise (InternalError "not in blockStructure")
  | S.Array es ->
    [Call (Some v, "allocate" ^ string_of_int (List.length es), List.map exp_to_atomic es)]

let op_to_test_op op =
  match op with
  | Tokens.Lt -> Lt
  | Tokens.Gt -> Gt
  | Tokens.Eq -> Eq
  | _ -> raise (InternalError "non-test operator in test in blockStructure")

let flat_exp_to_test (e : S.exp) : test =
  match e with
  | S.Ident (id, []) ->
    (Ident (id_to_var id), Eq, Num 1L)
  | S.Ident (id, _::_) ->
    raise (InternalError "array index in test position in blockStructure")
  | S.Num n ->
    raise (InternalError "number in test position in blockStructure")
  | S.Bool b ->
    (bool_to_num b, Eq, Num 1L)
  | S.Op (ae1, op, ae2) ->
    (exp_to_atomic ae1, op_to_test_op op, exp_to_atomic ae2)
  | S.Uop (Tokens.Not, ae) ->
    raise (InternalError "! in blockStructure")
  | S.Array es ->
    raise (InternalError "array alloc test in blockStructure")

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

  (* Convert stmts to basic blocks, and add them to the_cfg.
     block_num is the index for the block being created.
     block_acc contains the elements seen so far, in *reverse* order.
     ret_block is the control flow out of the block being created.

     The AST must be in a restricted form:
     - The expressions must all be unnested
       (i.e. UnnestExp.is_flat returns true).
     - && and || must have been removed.
     - Multi-dimensional array indexing must have been removed. *)
  let rec find_blocks (block_num : int) (block_acc : basic_block)
      (ret_block : next_block) (stmts : S.stmt list) : unit =
    match stmts with
    | [] -> add_block block_num block_acc ret_block
    | S.Assign (x, [], e) :: s1 ->
      find_blocks block_num (flat_e_to_assign x e @ block_acc) ret_block s1
    | S.Assign (x, [ae], e) :: s1 ->
      let ae = exp_to_atomic ae in
      (* x[ae] := e --> tmp_var := *x;
                        assert ae < length_var;
                        *(x + (ae+1) * 8) := e *)
      let get_len = Ld (tmp_var, id_to_var x, Num 0L) in
      (* TODO assert ae < tmp_var *)
      let new_block_elems =
        (match ae with
         | Num n ->
           [St (id_to_var x, Num (Int64.shift_left (Int64.add n 1L) 3), exp_to_atomic e);
            get_len]
         | _ ->
           [St (id_to_var x, Ident tmp_var, exp_to_atomic e);
            AssignOp (tmp_var, Ident tmp_var, Tokens.Lshift, Num 3L);
            AssignOp (tmp_var, ae, Tokens.Plus, Num 1L);
            get_len])
      in
      find_blocks block_num (new_block_elems @ block_acc) ret_block s1
    | S.Assign (x, _::_::_, e) :: s1 ->
      raise (InternalError "multi-dimension array index in blockStructure")
    | S.Stmts s1 :: s2 ->
      (* Treat { s1 ... sn } s1' ... sn' as though it were
         s1 ... sn s1' ... sn' *)
      find_blocks block_num block_acc ret_block (s1 @ s2)
    | S.DoWhile (s0, e, s1) :: s2 ->
      let header_block_n = get_block_num () in
      let body_block_n = get_block_num () in
      add_block block_num block_acc (Next header_block_n);
      find_blocks body_block_n [] (Next header_block_n) [s1];
      (match (s2, ret_block) with
       | ([], Next i) ->
         find_blocks header_block_n []
           (Branch (flat_exp_to_test e, body_block_n, i))
           [s0]
       | _ ->
         let following_block_n = get_block_num () in
         find_blocks header_block_n []
           (Branch (flat_exp_to_test e, body_block_n, following_block_n))
           [s0];
         find_blocks following_block_n [] ret_block s2)
    | S.Ite (e, s1, s2) :: s3 ->
      let true_block_n = get_block_num () in
      let false_block_n = get_block_num () in
      add_block block_num block_acc
        (Branch (flat_exp_to_test e, true_block_n, false_block_n));
      if s3 = [] then
        (find_blocks true_block_n [] ret_block [s1];
         find_blocks false_block_n [] ret_block [s2])
      else
        let following_block_n = get_block_num () in
        find_blocks true_block_n [] (Next following_block_n) [s1];
        find_blocks false_block_n [] (Next following_block_n) [s2];
        find_blocks following_block_n [] ret_block s3
    | S.In x :: s ->
      find_blocks block_num (Call (Some (id_to_var x),"input", []) :: block_acc) ret_block s
    | S.Out x :: s ->
      find_blocks block_num (Call (None, "output", [Ident (id_to_var x)]) :: block_acc) ret_block s
    | ((S.Loc _) :: _) ->
      raise (InternalError "Loc in blockStructure")
  in

  let end_block_num = get_block_num () in
  (* Later on we rely on the starting block being #1 *)
  let init_block = get_block_num () in
  add_block end_block_num [] End;
  find_blocks init_block [] (Next end_block_num) stmts;
  !the_cfg
