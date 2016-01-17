open BlockStructure
open X86
module T = Tokens 

(*
  | Times
  | Div
  | Lt
  | Eq
  | And
  | Or
   *)

let tok_to_binop t =
  match t with
  | T.Plus -> Zadd
  | T.Minus -> Zsub
  | T.Lshift -> Zshl
  | T.BitOr -> Zor
  | _ -> assert false

let reg_numbers =
  [(0, RAX);
   (1, RBX);
   (2, RCX);
   (3, RDX);
   (4, R8);
   (5, R9);
   (6, R10);
   (7, R11);
   (8, R12);
   (9, R13);
   (10, R14)]

let var_to_rm v =
  match v with
  | Vreg i -> Zr (List.assoc i reg_numbers)
  | Stack i -> Zm (None, Some RSP, Some (Int64.of_int (8 * i)))
  | _ -> assert false

let build_to_reg_op op r ae =
  match (op, ae) with
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr), Num i) ->
    Zbinop (tok_to_binop op, Zrm_i (Zr r, i))
  | ((T.Plus | T.Minus | T.Lshift | T.BitOr), Ident v) ->
    Zbinop (tok_to_binop op, Zr_rm (r, var_to_rm v))

(* Assume that we have kept r15 free for scratch *)
let rec be_to_x86 be =
  match be with
  | AssignOp (v, ae1, op, ae2) ->
    (match (var_to_rm v, ae1) with
     | (Zr r1, Num imm2) ->
       (* r1 := imm2 op m/r3 --> mov r1, imm2; op r1, m/r3 *)
       [Zmov (Zrm_i (Zr r1, imm2)); 
        build_to_reg_op op r1 ae2]
     | (Zr r1, Ident var) ->
       (* r1 := m/r2 op m/r3/imm3 --> mov r1, m/r2; op r1, m/r3/imm3 *)
       [Zmov (Zr_rm (r1, var_to_rm v));
        build_to_reg_op op r1 ae2]
     | (Zm _ as m1, Num imm2) ->
       (* m1 := imm2 op m/r3 --> mov r15, imm2; op r15, m/r3; mov m1, r15 *)
       [Zmov (Zrm_i (Zr R15, imm2));
        build_to_reg_op op R15 ae2;
        Zmov (Zrm_r (m1, R15))]
     | (Zm _ as m1, Ident var) ->
       (* m1 := m/r2 op m/r3/imm3 --> mov r15, m/r2; op r15, m/r3/imm3; mov m1, r15 *)
       [Zmov (Zr_rm (R15, var_to_rm var));
        build_to_reg_op op R15 ae2;
        Zmov (Zrm_r (m1, R15))])
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
          [Zmov (Zr_rm (R15, m2));
           Zmov (Zrm_r (m1, R15))]))
