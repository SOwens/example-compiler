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

(* Convert linearised three-address code to x86-64 *)

exception TODO

open BlockStructure
open X86
module L = LineariseCfg
module T = Tokens

let tok_to_binop t =
  match t with
  | T.Plus -> Zadd
  | T.Minus -> Zsub
  | T.Lshift -> Zshl
  | T.BitOr -> Zor
  | T.BitAnd -> Zand
  | _ -> assert false

let num_regs = 11

(* Save RSP and RBP for stack stuff,
   save RAX and RDX for scratch, and index RAX by -1
   RAX and RDX are chosen because division needs them *)
let reg_numbers =
  [(-1, RAX);
   (0, RBX);
   (1, RCX);
   (2, RSI);
   (3, RDI);
   (4, R8);
   (5, R9);
   (6, R10);
   (7, R11);
   (8, R12);
   (9, R13);
   (10, R14);
   (11, R15)]

(* Assume that we have kept RAX free for scratch space *)
let r_scratch = RAX
let r_scratch2 = RDX

(* Convert a variable, which can be a register or stack slot, to a rm *)
let var_to_rm (v : var) : rm =
  match v with
  | Vreg i -> Zr (List.assoc i reg_numbers)
  | Stack i -> Zm (None, Some RBP, Some (Int64.of_int (-8 * (i+1))))
  | _ -> raise (Util.InternalError "named variable in instrSelX86")

let rm_rm_to_dest_src (dest_rm : rm) (src_rm : rm)
  : instruction list * dest_src =
  match (dest_rm, src_rm) with
  | (Zr r, _) -> ([], Zr_rm (r, src_rm))
  | (_, Zr r) -> ([], Zrm_r (dest_rm, r))
  | (Zm _, Zm _) ->
    ([Zmov (Zr_rm (r_scratch, src_rm))], Zrm_r (dest_rm, r_scratch))

let rm_ae_to_dest_src (dest_rm : rm) (src_ae : atomic_exp)
  : instruction list * dest_src =
  match src_ae with
  | Num i -> ([], Zrm_i (dest_rm, i))
  | Ident src_var -> rm_rm_to_dest_src dest_rm (var_to_rm src_var)

(* Convert a heap reference, which is a variable to be deferenced, offset by
   another variable or immediate *)
let heap_to_rm (base : var) (offset : atomic_exp) : instruction list * rm =
  match (base, offset) with
  | (Vreg b, Num o) ->
    ([], Zm (None, Some (List.assoc b reg_numbers), Some o))
  | (Vreg b, Ident (Vreg o)) ->
    ([],
     Zm (Some (1, List.assoc o reg_numbers),
         Some (List.assoc b reg_numbers),
         None))
  | (Vreg b, Ident (Stack i)) ->
    ([Zmov (Zr_rm (r_scratch2, var_to_rm (Stack i)))],
     Zm (Some (1, r_scratch2),
         Some (List.assoc b reg_numbers),
         None))
  | (Stack i, Num o) ->
    ([Zmov (Zr_rm (r_scratch2, var_to_rm (Stack i)))],
     Zm (None, Some r_scratch2, Some o))
  | (Stack i, Ident (Vreg r)) ->
    ([Zmov (Zr_rm (r_scratch2, var_to_rm (Stack i)))],
     Zm (Some (1, List.assoc r reg_numbers),
         Some r_scratch2,
         None))
  | (Stack i, Ident (Stack j)) ->
    ([Zmov (Zr_rm (r_scratch2, var_to_rm (Stack i)));
      Zbinop (Zadd, Zr_rm (r_scratch2, var_to_rm (Stack j)))],
     Zm (None,
         Some r_scratch2,
         None))
  | ((NamedSource _ | NamedTmp _), _) ->
    raise (Util.InternalError "Named variables in instrSelX86")
  | (_, Ident (NamedSource _ | NamedTmp _)) ->
    raise (Util.InternalError "Named variables in instrSelX86")

(* Build the operation for r := r op ae *)
let build_to_reg_op (op : Tokens.op) (r : reg) (ae : atomic_exp)
  : instruction list =
  match (op, ae) with
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr | T.BitAnd), Num i) ->
    [Zbinop (tok_to_binop op, Zrm_i (Zr r, i))]
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr | T.BitAnd), Ident v) ->
    [Zbinop (tok_to_binop op, Zr_rm (r, var_to_rm v))]
  | (T.Times, Num i) ->
    [Zimul (r, Zr r, Some i)]
  | (T.Times, Ident v) ->
    [Zimul (r, var_to_rm v, None)]
  | (T.Div, Ident v) ->
    [Zmov (Zrm_i (Zr RDX, 0L));
     Zmov (Zr_rm (RAX, Zr r));
     Zidiv (var_to_rm v);
     Zmov (Zr_rm (r, Zr r_scratch))]
  | (T.Div, Num i) ->
    [Zmov (Zrm_i (Zr RDX, 0L));
     Zmov (Zr_rm (RAX, Zr r));
     Zmov (Zrm_i (Zr r, i));
     Zidiv (Zr r);
     Zmov (Zr_rm (r, Zr r_scratch))]
  | ((T.Lt | T.Gt | T.Eq), _) ->
    assert false
  | ((T.And | T.Or), _) ->
    assert false

let reverse_op op =
  match op with
  | T.Gt -> T.Lt
  | T.Lt -> T.Gt
  | T.Eq -> T.Eq
  | _ -> assert false

let op_to_cond op =
  match op with
  | T.Gt -> Z_G
  | T.Lt -> Z_L
  | T.Eq -> Z_E
  | _ -> assert false

(* Don't save RAX since it is our scratch. Remember to do an even number before
   a call for alignment. This is why we also push RDX, even though it is
   scratch too. *)
let caller_save =
  [Zpush (Zi_rm (Zr RCX));
   Zpush (Zi_rm (Zr RDX));
   Zpush (Zi_rm (Zr RSI));
   Zpush (Zi_rm (Zr RDI));
   Zpush (Zi_rm (Zr R8));
   Zpush (Zi_rm (Zr R9));
   Zpush (Zi_rm (Zr R10));
   Zpush (Zi_rm (Zr R11))]

let caller_restore =
  [Zpop (Zr R11);
   Zpop (Zr R10);
   Zpop (Zr R9);
   Zpop (Zr R8);
   Zpop (Zr RDI);
   Zpop (Zr RSI);
   Zpush (Zi_rm (Zr RDX));
   Zpop (Zr RCX)]

let reg_list = [RDI; RSI; RDX; RCX; R8; R9]

let stack_reg_to_offset r =
  match r with
  | RDI -> 32L
  | RSI -> 40L
  | RDX -> 48L
  | RCX -> 56L
  | R8 -> 24L
  | _ -> assert false

(* TODO rename and refactor *)
let reg_ae_to_dest_src (overwritten_regs : reg list) dest_r ae =
  match ae with
  | Num n -> Some (Zrm_i (Zr dest_r, n))
  | Ident (Stack i) -> Some (Zr_rm (dest_r, var_to_rm (Stack i)))
  | Ident (Vreg src_r) ->
    let src_r = List.assoc src_r reg_numbers in
    if src_r = dest_r then
      None
    else if List.mem src_r overwritten_regs then
      Some (Zr_rm (dest_r,
                   Zm (None, Some RSP, Some (stack_reg_to_offset src_r))))
    else
      Some (Zr_rm (dest_r, Zr src_r))
  | Ident (NamedSource _ | NamedTmp _) ->
    raise (Util.InternalError "Named variable in instrSelX86")

let rec setup_args (aes : atomic_exp list) remaining_regs overwritten_regs
  : instruction list =
  match (aes, remaining_regs) with
  | ([], _) -> []
  | (a :: aes, next :: regs) ->
    (match reg_ae_to_dest_src overwritten_regs next a with
     | None ->
       []
     | Some arg ->
       [Zmov arg]) @
    setup_args aes regs (next :: overwritten_regs)
  | _ -> raise TODO

let rec be_to_x86 (underscore_labels : bool) be : instruction list =
  match be with
  | AssignOp (v, Num imm, ((T.Lt | T.Gt | T.Eq) as op), ae2) ->
    (* constant prop ensures both aren't immediate *)
    be_to_x86 underscore_labels (AssignOp (v, ae2, reverse_op op, Num imm))
  | AssignOp (v, Ident v2, ((T.Lt | T.Gt | T.Eq) as op), ae2) ->
    let (instrs, cmp_arg) = rm_ae_to_dest_src (var_to_rm v2) ae2 in
    let cmp_instrs = instrs @ [Zbinop (Zcmp, cmp_arg)] in
    (match var_to_rm v with
     | Zm _ as m ->
       cmp_instrs @
       [Zset (op_to_cond op, B r_scratch);
        Zbinop (Zand, Zrm_i (Zr r_scratch, 1L));
        Zmov (Zrm_r (m, r_scratch))]
     | Zr r ->
       cmp_instrs @
       [Zset (op_to_cond op, B r);
        Zbinop (Zand, Zrm_i (Zr r, 1L))])
  | AssignOp (v, ae1, ((T.Plus | T.Minus | T.Lshift | T.BitOr
                       | T.BitAnd | T.Times | T.Div) as op), ae2) ->
    (match (var_to_rm v, ae1) with
     | (Zr r1, Num imm2) ->
       (* r1 := imm2 op m/r3 --> mov r1, imm2; op r1, m/r3 *)
       Zmov (Zrm_i (Zr r1, imm2)) :: build_to_reg_op op r1 ae2
     | (Zr r1, Ident var) ->
       (* r1 := m/r2 op m/r3/imm3 --> mov r1, m/r2; op r1, m/r3/imm3 *)
       Zmov (Zr_rm (r1, var_to_rm var)) :: build_to_reg_op op r1 ae2
     | (Zm _ as m1, Num imm2) ->
       (* m1 := imm2 op m/r3 -->
          mov r_scratch, imm2; op r_scratch, m/r3; mov m1, r_scratch *)
       Zmov (Zrm_i (Zr r_scratch, imm2)) ::
       build_to_reg_op op r_scratch ae2 @
       [Zmov (Zrm_r (m1, r_scratch))]
     | (Zm _ as m1, Ident var) ->
       (* m1 := m/r2 op m/r3/imm3 -->
          mov r_scratch, m/r2; op r_scratch, m/r3/imm3; mov m1, r_scratch *)
       Zmov (Zr_rm (r_scratch, var_to_rm var)) ::
       build_to_reg_op op r_scratch ae2 @
       [Zmov (Zrm_r (m1, r_scratch))])
  | AssignOp (_, _, (T.And | T.Or), _) ->
    raise (Util.InternalError "And/Or in instrSelX86")
  | AssignAtom (v, ae) ->
    let (instrs, mov_arg) = rm_ae_to_dest_src (var_to_rm v) ae in
    instrs @ [Zmov mov_arg]
  | Ld (v1, v2, ae) ->
    let (instrs, src_rm) = heap_to_rm v2 ae in
    let (instrs2, dest_src) = rm_rm_to_dest_src (var_to_rm v1) src_rm in
    instrs @
    instrs2 @
    [Zmov dest_src]
  | St (v, ae1, ae2) ->
    let (instrs, dest_rm) = heap_to_rm v ae1 in
    let (instrs2, dest_src) = rm_ae_to_dest_src dest_rm ae2 in
    instrs @
    instrs2 @
    [Zmov dest_src]
  | In v ->
    caller_save @
    [Zcall ((if underscore_labels then "_" else "") ^ "input")] @
    caller_restore @
    [Zmov (Zrm_r (var_to_rm v, r_scratch))]
  | Out v ->
    caller_save @
    [Zmov (Zr_rm (RDI, var_to_rm v));
     Zcall ((if underscore_labels then "_" else "") ^ "output")] @
    caller_restore
  | Alloc (v, aes) ->
    let alloc_name =
      (if underscore_labels then "_" else "") ^
      "allocate" ^
      string_of_int (List.length aes)
    in
    caller_save @
    setup_args aes reg_list [] @
    [Zcall alloc_name] @
    caller_restore @
    [Zmov (Zrm_r (var_to_rm v, r_scratch))]

(* Return a boolean true if the condition needs to be negated *)
let test_to_x86 ae1 ae2 : instruction * bool =
  match (ae1, ae2) with
  | (Ident i, Num imm) ->
    (Zbinop (Zcmp, Zrm_i (var_to_rm i, imm)), false)
  | (Num imm, Ident i) ->
    (Zbinop (Zcmp, Zrm_i (var_to_rm i, imm)), true)
  | (Ident i1, Ident i2) ->
    (match (var_to_rm i1, var_to_rm i2) with
     | (Zr r1, Zr r2) ->
       (Zbinop (Zcmp, Zrm_r (Zr r1, r2)), false)
     | (Zm _ as m, Zr r) ->
       (Zbinop (Zcmp, Zrm_r (m, r)), false)
     | (Zr r, (Zm _ as m)) ->
       (Zbinop (Zcmp, Zr_rm (r, m)), false)
     | (Zm _, Zm _) ->
       raise TODO)
  | (Num _, Num _) ->
    raise (Util.InternalError "2 immediates in instrSelX86")

let op_to_cc b op =
  match (b, op) with
  | (true, Lt) ->
    Z_L
  | (false, Lt) ->
    Z_NL
  | (true, Gt) ->
    Z_G
  | (false, Gt) ->
    Z_NG
  | (true, Eq) ->
    Z_E
  | (false, Eq) ->
    Z_NE

let reverse_op2 op =
  match op with
  | Gt -> Lt
  | Lt -> Gt
  | Eq -> Eq

let to_x86 (underscore_labels : bool) (ll : L.linear list) (num_stack : int)
  : instruction list =
  (* We have to keep RSP 16 byte aligned, add a qword if necessary *)
  let num_stack =
    if num_stack mod 2 = 0 then
      num_stack
    else
      num_stack + 1
  in
  Zpush (Zi_rm (Zr RBP)) ::
  Zmov (Zr_rm (RBP, Zr RSP)) ::
  Zbinop (Zsub, Zrm_i (Zr RSP, Int64.mul (Int64.of_int num_stack) 8L)) ::
  List.flatten
    (List.map
       (fun l ->
          match l with
          | L.Instr be -> be_to_x86 underscore_labels be
          | L.CJump ((ae1, op, ae2), b, s) ->
            let (cmp, reverse) = test_to_x86 ae1 ae2 in
            [cmp;
             Zjcc (op_to_cc b (if reverse then reverse_op2 op else op), s)]
          | L.Jump s ->
            [Zjcc (Z_ALWAYS, s)]
          | L.Label s ->
            [Zlabel s])
       ll)
