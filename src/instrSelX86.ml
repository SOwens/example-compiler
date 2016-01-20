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

(* Save RSP for stack stuff,
   save RAX for scratch, and index it by -1
   very pessimistically save RDX for division *)
let reg_numbers =
  [(-1, RAX);
   (0, RBX);
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
  | Stack i -> Zm (None, Some RBP, Some (Int64.of_int (-8 * (i+1))))
  | _ -> assert false (* Register allocation should have removed all named variables *)

(* Build the operation for r := r op ae *)
let build_to_reg_op op r ae =
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
     Zmov (Zr_rm (r, Zr RAX))]
  | (T.Div, Num i) ->
    [Zmov (Zrm_i (Zr RDX, 0L));
     Zmov (Zr_rm (RAX, Zr r));
     Zmov (Zrm_i (Zr r, i));
     Zidiv (Zr r);
     Zmov (Zr_rm (r, Zr RAX))]
  | ((T.Lt | T.Gt | T.Eq), _) -> 
    assert false
  | ((T.And | T.Or), _) -> 
    assert false

(* Assume that we have kept RAX free for scratch space *)
let r_scratch = RAX

let invert_op op =
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

(* Don't save RAX, since it is our scratch. Remember to do an even number
   before a call for alignment *)
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
   Zpop (Zr RDX);
   Zpop (Zr RCX)]

let rec be_to_x86 be =
  match be with
  | AssignOp (v, Num imm, ((T.Lt | T.Gt | T.Eq) as op), ae2) ->
    be_to_x86 (AssignOp (v, ae2, invert_op op, Num imm)) (* constant prop ensures both aren't immediate *)
  | AssignOp (v, Ident v2, ((T.Lt | T.Gt | T.Eq) as op), ae2) ->
    let cmp_instr = 
      match ae2 with 
      | Num i -> 
        [Zbinop (Zcmp, Zrm_i (var_to_rm v2, i))]
      | Ident v3 ->
        (match (var_to_rm v2, var_to_rm v3) with
         | (rm2, Zr r3) ->
           [Zbinop (Zcmp, Zrm_r (rm2, r3))]
         | (Zr r2, rm3) ->
           [Zbinop (Zcmp, Zr_rm (r2, rm3))]
         | ((Zm _ as m2), (Zm _ as m3)) ->
           [Zmov (Zr_rm (r_scratch, m2));
            Zbinop (Zcmp, Zr_rm (r_scratch, m3))])
    in
    (match var_to_rm v with
     | Zm _ as m ->
       cmp_instr @
       [Zset (op_to_cond op, B r_scratch);
        Zbinop (Zand, Zrm_i (Zr r_scratch, 1L));
        Zmov (Zrm_r (m, r_scratch))]
     | Zr r ->
       cmp_instr @
       [Zset (op_to_cond op, B r);
        Zbinop (Zand, Zrm_i (Zr r, 1L))])
  | AssignOp (v, ae1, ((T.Plus | T.Minus | T.Lshift | T.BitOr | T.BitAnd | T.Times | T.Div) as op), ae2) ->
    (match (var_to_rm v, ae1) with
     | (Zr r1, Num imm2) ->
       (* r1 := imm2 op m/r3 --> mov r1, imm2; op r1, m/r3 *)
       Zmov (Zrm_i (Zr r1, imm2)) :: build_to_reg_op op r1 ae2
     | (Zr r1, Ident var) ->
       (* r1 := m/r2 op m/r3/imm3 --> mov r1, m/r2; op r1, m/r3/imm3 *)
       Zmov (Zr_rm (r1, var_to_rm var)) :: build_to_reg_op op r1 ae2
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
  | AssignOp (_, _, (T.And | T.Or), _) ->
    assert false (* Should have been removed in the removeBool phase *)
  | AssignAtom (v, ae) ->
    (* Essentially special casing the AssignOp case above *)
    (match (var_to_rm v, ae) with
     | (rm1, Num i) ->
       [Zmov (Zrm_i (rm1, i))]
     | (Zr r1, Ident v) ->
       [Zmov (Zr_rm (r1, var_to_rm v))]
     | (Zm _ as m1, Ident v2) ->
       (match var_to_rm v2 with
        | Zr r2 ->
          [Zmov (Zrm_r (m1, r2))]
        | Zm _ as m2 ->
          (* Can't do a memory-to-memory move *)
          [Zmov (Zr_rm (r_scratch, m2));
           Zmov (Zrm_r (m1, r_scratch))]))
  | Ld _ | St _ -> assert false (* TODO *)
  | In v ->
    caller_save @ 
    [Zcall "_input"] @
    caller_restore @
    [Zmov (Zrm_r (var_to_rm v, RAX))]
  | Out v -> 
    caller_save @ 
    [Zmov (Zr_rm (RDI, var_to_rm v));
     Zcall "_output"] @
    caller_restore

let to_x86 (ll : L.linear list) (num_stack : int) : instruction list =
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
          | L.Instr be -> be_to_x86 be
          | L.CJump (v, b, s) ->
            [Zbinop (Zcmp, Zrm_i (var_to_rm v, 0L));
             Zjcc ((if b then Z_NE else Z_E), s)]
          | L.Jump s ->
            [Zjcc (Z_ALWAYS, s)]
          | L.Label s ->
            [Zlabel s])
       ll)
