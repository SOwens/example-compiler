(* Ensure that all immediate arguments fit into 32 bits.  We assume that
   constant propagation has ensured that no operation has two immediate
   arguments, and we maintain that property here. *)

open BlockStructure

(* Build a series of assignments that puts the immediate n into the dest register, using
   only immediates of 32 bits or smaller *)
let assign_imm (dest : 'reg) (n : Int64.t) : 'reg block_elem list =
  [AssignAtom (dest, Num (Int64.shift_right_logical n 32));
   AssignOp (dest, Ident dest, Tokens.Lshift, Num 32L);
   AssignOp (dest, Ident dest, Tokens.BitOr, Num (Int64.logand n 0x00000000FFFFFFFFL))]

let is_imm (a : 'reg atomic_exp) : bool =
  match a with
  | Ident _ -> false
  | Num _ | Bool _ -> true

let get_large_imm (a : 'reg atomic_exp) : Int64.t option =
  match a with
  | Num n ->
    let topmost = Int64.shift_right n 31 in
    if Int64.compare topmost 0L = 0 ||
       Int64.compare topmost 0x1FFFFFFFFL = 0 then
      None
    else 
      Some n
  | _ -> None

let shrink_imm_elem (tmp_reg : 'reg) (e : 'reg block_elem) : 'reg block_elem list =
  match e with
  | AssignOp (dest, a1, op, a2) ->
    assert (not (is_imm a1 && is_imm a2));
    begin
      match get_large_imm a1 with
      | Some n ->
        assign_imm tmp_reg n @
        [AssignOp (dest, Ident tmp_reg, op, a2)]
      | None ->
        begin
          match get_large_imm a2 with
          | Some n -> 
            assign_imm tmp_reg n @
            [AssignOp (dest, a1, op, Ident tmp_reg)]
          | None -> 
            [e]
        end
    end
  | AssignAtom (dest, a) ->
    begin
      match get_large_imm a with
      | Some n -> assign_imm dest n
      | None -> [e]
    end
  | Ld (r, addr) -> [e]
  | St (r, addr) -> [e]
  | In r -> [e]
  | Out r -> [e]

let shrink_imm (cfg : string cfg) : string cfg =
  List.map 
    (fun cfg_entry ->
       { cfg_entry with elems = List.flatten (List.map (shrink_imm_elem "__tmp") cfg_entry.elems) })
    cfg
