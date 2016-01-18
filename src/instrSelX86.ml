open BlockStructure
open X86
module T = Tokens 

(*
  | Times
  | Div
  | Lt
  | Eq
   *)

let tok_to_binop t =
  match t with
  | T.Plus -> Zadd
  | T.Minus -> Zsub
  | T.Lshift -> Zshl
  | T.BitOr -> Zor
  | _ -> assert false

(* Save RSP for stack stuff,
   save RAX for scratch, 
   very pessimistically save RDX for division *)
let reg_numbers =
  [(0, RBX);
   (1, RCX);
   (2, R8);
   (3, R9);
   (4, R10);
   (5, R11);
   (6, R12);
   (7, R13);
   (8, R14);
   (9, R15)]

let var_to_rm v =
  match v with
  | Vreg i -> Zr (List.assoc i reg_numbers)
  | Stack i -> Zm (None, Some RSP, Some (Int64.of_int (8 * i)))
  | _ -> assert false (* Register allocation should have removed all named variables *)

(* Build the operation for r := r op ae *)
let build_to_reg_op op r ae =
  match (op, ae) with
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr), Num i) ->
    [Zbinop (tok_to_binop op, Zrm_i (Zr r, i))]
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr), Ident v) ->
    [Zbinop (tok_to_binop op, Zr_rm (r, var_to_rm v))]
  | (T.Times, Num i) ->
    [Zimul (r, Zr r, Some i)]
  | (T.Times, Ident v) ->
    [Zimul (r, var_to_rm v, None)]
  | (T.Div, Ident v) ->
    [Zmov (Zrm_i (Zr RDX, 0L));
     Zmov (Zr_rm (RAX, Zr r));
     Zidiv (var_to_rm v);
     Zmov (Zr_rm (r, Zr RAX))]
  | (T.Div, Num i) ->
    [Zmov (Zrm_i (Zr RDX, 0L));
     Zmov (Zr_rm (RAX, Zr r));
     Zmov (Zrm_i (Zr r, i));
     Zidiv (Zr r);
     Zmov (Zr_rm (r, Zr RAX))]
  | ((T.Lt | T.Eq), _) -> assert false (* TODO *)
  | ((T.And | T.Or), _) -> assert false (* Should have been removed in the removeBool phase *)

(* Assume that we have kept RAX free for scratch space *)
let r_scratch = RAX

let rec be_to_x86 be =
  match be with
  | AssignOp (v, ae1, op, ae2) ->
    (match (var_to_rm v, ae1) with
     | (Zr r1, Num imm2) ->
       (* r1 := imm2 op m/r3 --> mov r1, imm2; op r1, m/r3 *)
       Zmov (Zrm_i (Zr r1, imm2)) :: build_to_reg_op op r1 ae2
     | (Zr r1, Ident var) ->
       (* r1 := m/r2 op m/r3/imm3 --> mov r1, m/r2; op r1, m/r3/imm3 *)
       Zmov (Zr_rm (r1, var_to_rm v)) :: build_to_reg_op op r1 ae2
     | (Zm _ as m1, Num imm2) ->
       (* m1 := imm2 op m/r3 --> mov r_scratch, imm2; op r_scratch, m/r3; mov m1, r_scratch *)
       Zmov (Zrm_i (Zr r_scratch, imm2)) ::
       build_to_reg_op op r_scratch ae2 @
       [Zmov (Zrm_r (m1, r_scratch))]
     | (Zm _ as m1, Ident var) ->
       (* m1 := m/r2 op m/r3/imm3 --> mov r_scratch, m/r2; op r_scratch, m/r3/imm3; mov m1, r_scratch *)
       Zmov (Zr_rm (r_scratch, var_to_rm var)) ::
       build_to_reg_op op r_scratch ae2 @
       [Zmov (Zrm_r (m1, r_scratch))])
  | AssignAtom (v, ae) ->
    (* Essentially special casing the AssignOp case above *)
    (match (var_to_rm v, ae) with
     | (Zr r1, Num i) ->
       [Zmov (Zrm_i (Zr r1, i))]
     | (Zr r1, Ident v) ->
       [Zmov (Zr_rm (r1, var_to_rm v))]
     | (Zm _ as m1, Num i) ->
       [Zmov (Zrm_i (m1, i))]
     | (Zm _ as m1, Ident v2) ->
       (match var_to_rm v2 with
        | Zr r2 ->
          [Zmov (Zrm_r (m1, r2))]
        | Zm _ as m2 ->
          (* Can't do a memory-to-memory move *)
          [Zmov (Zr_rm (r_scratch, m2));
           Zmov (Zrm_r (m1, r_scratch))]))
  | Ld _ | St _ -> assert false (* TODO *)
  | In var -> assert false (* TODO *)
  | Out var -> assert false (* TODO *)

