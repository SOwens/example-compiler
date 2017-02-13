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

(* A control flow graph representation with basic blocks, and the source AST ->
   CGF algorithm. Also compiles arrays to loads and stores. *)

open Util
module S = SourceAst

type var =
  | Vreg of int
  | Stack of int
  | Global of string
  | NamedSource of string * SourceAst.scope
  | NamedTmp of string * int

let var_cmp_index (v : var) : int =
  match v with
  | Vreg _ -> 0
  | Stack _ -> 1
  | Global _ -> 2
  | NamedSource _ -> 3
  | NamedTmp _ -> 4

let compare_var v1 v2 =
  match (v1, v2) with
  | (Vreg i1, Vreg i2) -> compare i1 i2
  | (Stack i1, Stack i2) -> compare i1 i2
  | (Global s1, Global s2) -> String.compare s1 s2
  | (NamedSource (s1, scope1), NamedSource (s2, scope2)) ->
    let c = SourceAst.compare_scope scope1 scope2 in
    if c = 0 then
      String.compare s1 s2
    else
      c
  | (NamedTmp (s1, i1), NamedTmp (s2, i2)) ->
    let c = String.compare s1 s2 in
    if c = 0 then
      compare i1 i2
    else
      c
  | _ -> compare (var_cmp_index v1) (var_cmp_index v2)

let show_var v =
  match v with
  | Vreg i -> "_r_" ^ string_of_int i
  | Stack i -> "_s_" ^ string_of_int i
  | Global i -> "_g_" ^ i
  | NamedSource (s,_) -> s
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
  let show s = show_list show_var (elements s)
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
  | Num i -> Int64.to_string i

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
  (* Call (x, f, aes) represents x := f(aes) *)
  | Call of var option * string * atomic_exp list
  (* BoundCheck (a1, a2) represents assert (a1 >= 0 && a1 < a2) *)
  | BoundCheck of atomic_exp * atomic_exp
  (* NullCheck v represents assert (v <> 0) *)
  | NullCheck of var

let pp_block_elem fmt be =
  match be with
  | AssignOp (v, ae1, op, ae2) ->
    Format.fprintf fmt "%a := %a %s %a"
      pp_var v
      pp_atomic_exp ae1
      (Tokens.show_op op)
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
  | BoundCheck (a1, a2) ->
    Format.fprintf fmt "assert (%a >= 0 && %a < %a)"
      pp_atomic_exp a1
      pp_atomic_exp a1
      pp_atomic_exp a2
  | NullCheck v ->
    Format.fprintf fmt "assert (%a <> 0)"
      pp_var v


let show_block_elem be =
  pp_block_elem Format.str_formatter be;
  Format.flush_str_formatter ()

type basic_block = block_elem list

type test_op =
  | Lt
  | Gt
  | Eq

let pp_test_op fmt op =
  match op with
  | Lt -> Format.fprintf fmt "<"
  | Gt -> Format.fprintf fmt ">"
  | Eq -> Format.fprintf fmt "="

type test = atomic_exp * test_op * atomic_exp

let pp_test fmt (ae1, op, ae2) =
  Format.fprintf fmt "%a %a %a"
    pp_atomic_exp ae1
    pp_test_op op
    pp_atomic_exp ae2

(* A basic block is either at the end of the function, returning a var, or
   there is an unconditional jump out of it, or a branch out of it to the
   blocks indexed by the int. *)
type next_block =
  | Return of var option
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of test * int * int

let pp_next_block fmt nb =
  match nb with
  | Return (Some v) -> Format.fprintf fmt "return %a" pp_var v
  | Return None -> Format.fprintf fmt "return"
  | Next i -> Format.fprintf fmt "B%d" i
  | Branch (t,i1,i2) -> Format.fprintf fmt "if@ %a@ then@ B%d@ else@ B%d"
                          pp_test t
                          i1
                          i2

(* An adjacency list representation for the CFG *)
type cfg_entry =
  { bnum : int; elems : block_elem list; next : next_block;
    mutable started : bool; mutable finished : bool }

let pp_cfg_entry fmt e =
  Format.fprintf fmt "@[[B%d:@ %a@ %a]@]"
    e.bnum
    (pp_list pp_block_elem) e.elems
    pp_next_block e.next

type cfg = cfg_entry list

let pp_cfg fmt es = pp_list pp_cfg_entry fmt es

let cfg_to_graphviz fmt (cfg : cfg) : unit =
  Format.fprintf fmt "digraph {@\nnode[shape=box]@\n%a@\n}"
    (fun fmt cfg ->
       List.iter
         (fun entry ->
            Format.fprintf fmt "%d[label=\"%a\"]"
              entry.bnum
              (fun fmt e ->
                 if entry.bnum = 0 then
                   Format.fprintf fmt "ENTRY\\l"
                 else
                   ();
                 List.iter (fun x -> Format.fprintf fmt "%s\\l" (show_block_elem x))
                   e.elems;
                 match e.next with
                 | Branch (t,_,_) -> Format.fprintf fmt "%a\\l" pp_test t
                 | Return None -> Format.fprintf fmt "RETURN\\l"
                 | Return (Some i) -> Format.fprintf fmt "RETURN %s\\l" (show_var i)
                 | _ -> ())
              entry;
            Format.fprintf fmt "@\n";
            match entry.next with
            | Return _ -> ()
            | Next i ->
              Format.fprintf fmt "%d->%d@\n" entry.bnum i
            | Branch (_, i1, i2) ->
              Format.fprintf fmt "%d->%d[label=1]@\n" entry.bnum i1;
              Format.fprintf fmt "%d->%d[label=0]@\n" entry.bnum i2)
         cfg)
    cfg

let id_to_var (i : S.id) : var =
  match i with
  | S.Source (s, None) ->
    raise (InternalError ("un-scoped source identifier in blockStructure: " ^
                          S.show_id (S.Source (s, None))))
  | S.Source (s, Some scope) -> NamedSource (s, scope)
  | S.Temp (s, i) -> NamedTmp (s, i)

let bool_to_num b =
  if b then Num 1L else Num 0L

(* Convert an amomic source expression *)
let exp_to_atomic (e : S.exp) : atomic_exp =
  match e with
  | S.Ident (id, []) -> Ident (id_to_var id)
  | S.Num n -> Num n
  | S.Bool b -> bool_to_num b
  | S.Ident (_, _::_) | S.Op _ | S.Uop _ | S.Array _ | S.Call _ ->
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
    (match ae with
     | Num n ->
       [Ld (v, id_to_var id, Num (Int64.shift_left (Int64.add n 1L) 3));
        BoundCheck (ae, Ident tmp_var);
        get_len;
        NullCheck (id_to_var id)]
     | _ ->
       [Ld (v, id_to_var id, Ident tmp_var);
        AssignOp (tmp_var, Ident tmp_var, Tokens.Lshift, Num 3L);
        AssignOp (tmp_var, ae, Tokens.Plus, Num 1L);
        BoundCheck (ae, Ident tmp_var);
        get_len;
        NullCheck (id_to_var id)])
  | S.Ident (_, _::_::_) ->
    raise (InternalError "multi-dimension array index in blockStructure")
  | S.Num n -> [AssignAtom (v, Num n)]
  | S.Bool b -> [AssignAtom (v, bool_to_num b)]
  | S.Op (ae1, op, ae2) ->
    [AssignOp (v, exp_to_atomic ae1, op, exp_to_atomic ae2)]
  | S.Uop (Tokens.Not, ae) ->
    (* !x == (x = false) *)
    [AssignOp (v, exp_to_atomic ae, Tokens.Eq, Num 0L)]
  | S.Array es ->
    [Call (Some v, "allocate" ^ string_of_int (List.length es),
           List.map exp_to_atomic es)]
  | S.Call (f, es) ->
    [Call (Some v, S.show_id f, List.map exp_to_atomic es)]

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
  | S.Ident (_, _::_) ->
    raise (InternalError "array index in test position in blockStructure")
  | S.Num _ ->
    raise (InternalError "number in test position in blockStructure")
  | S.Bool b ->
    (bool_to_num b, Eq, Num 1L)
  | S.Op (ae1, op, ae2) ->
    (exp_to_atomic ae1, op_to_test_op op, exp_to_atomic ae2)
  | S.Uop (Tokens.Not, ae) ->
    (* !x == (x = false) *)
    (exp_to_atomic ae, Eq, Num 0L)
  | S.Array _ ->
    raise (InternalError "array alloc test in blockStructure")
  | S.Call _ ->
    raise (InternalError "function call test in blockStructure")

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
     - Multi-dimensional array indexing must have been removed.
     - No statements can follow a return
     - All paths must terminate in a return *)
  let rec find_blocks (block_num : int) (block_acc : basic_block)
      (following_block : next_block) (stmts : S.stmt list) : unit =
    match stmts with
    | [] -> add_block block_num block_acc following_block
    | S.Assign (x, [], e) :: s1 ->
      find_blocks block_num (flat_e_to_assign x e @ block_acc) following_block s1
    | S.Assign (x, [ae], e) :: s1 ->
      let ae = exp_to_atomic ae in
      (* x[ae] := e --> tmp_var := *x;
                        assert ae < length_var;
                        *(x + (ae+1) * 8) := e *)
      let get_len = Ld (tmp_var, id_to_var x, Num 0L) in
      let new_block_elems =
        (match ae with
         | Num n ->
           [St (id_to_var x, Num (Int64.shift_left (Int64.add n 1L) 3),
                exp_to_atomic e);
            BoundCheck (ae, Ident tmp_var);
            get_len;
            NullCheck (id_to_var x)]
         | _ ->
           [St (id_to_var x, Ident tmp_var, exp_to_atomic e);
            AssignOp (tmp_var, Ident tmp_var, Tokens.Lshift, Num 3L);
            AssignOp (tmp_var, ae, Tokens.Plus, Num 1L);
            BoundCheck (ae, Ident tmp_var);
            get_len;
            NullCheck (id_to_var x)])
      in
      find_blocks block_num (new_block_elems @ block_acc) following_block s1
    | S.Assign (_, _::_::_, _) :: _ ->
      raise (InternalError "multi-dimension array index in blockStructure")
    | S.Stmts s1 :: s2 ->
      (* Treat { s1 ... sn } s1' ... sn' as though it were
         s1 ... sn s1' ... sn' *)
      find_blocks block_num block_acc following_block (s1 @ s2)
    | S.DoWhile (s0, e, s1) :: s2 ->
      let header_block_n = get_block_num () in
      let body_block_n = get_block_num () in
      add_block block_num block_acc (Next header_block_n);
      find_blocks body_block_n [] (Next header_block_n) [s1];
      (match (s2, following_block) with
       | ([], Next i) ->
         find_blocks header_block_n []
           (Branch (flat_exp_to_test e, body_block_n, i))
           [s0]
       | _ ->
         let following_block_n = get_block_num () in
         find_blocks header_block_n []
           (Branch (flat_exp_to_test e, body_block_n, following_block_n))
           [s0];
         find_blocks following_block_n [] following_block s2)
    | S.Ite (e, s1, s2) :: s3 ->
      let true_block_n = get_block_num () in
      let false_block_n = get_block_num () in
      add_block block_num block_acc
        (Branch (flat_exp_to_test e, true_block_n, false_block_n));
      if s3 = [] then
        (find_blocks true_block_n [] following_block [s1];
         find_blocks false_block_n [] following_block [s2])
      else
        let following_block_n = get_block_num () in
        find_blocks true_block_n [] (Next following_block_n) [s1];
        find_blocks false_block_n [] (Next following_block_n) [s2];
        find_blocks following_block_n [] following_block s3
    | S.In x :: s ->
      find_blocks block_num (Call (Some (id_to_var x),"input", []) :: block_acc)
        following_block s
    | S.Out x :: s ->
      find_blocks block_num
        (Call (None, "output", [Ident (id_to_var x)]) :: block_acc) following_block s
    | ((S.Loc (s1, _)) :: s2) ->
      find_blocks block_num block_acc following_block (s1::s2)
    | [S.Return (Some x)] ->
      add_block block_num block_acc (Return (Some (id_to_var x)))
    | [S.Return None] ->
      add_block block_num block_acc (Return None)
    | S.Return _ :: _ ->
      raise (InternalError "return followed by statements in blockStructure")

  in

  (* Later on we rely on the starting block being #0 *)
  let init_block = get_block_num () in
  find_blocks init_block [] (Return None) stmts;
  !the_cfg
