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

(* Ensure that all immediate arguments fit into 32 bits.  We assume that
   constant propagation has ensured that no operation has two immediate
   arguments, and we maintain that property here. *)

open BlockStructure

(* Build a series of assignments that puts the immediate n into the dest register, using
   only immediates of 32 bits or smaller *)
let assign_imm (dest : var) (n : int64) : block_elem list =
  [AssignAtom (dest, Num (Int64.shift_right_logical n 32));
   AssignOp (dest, Ident dest, Tokens.Lshift, Num 32L);
   AssignOp (dest, Ident dest, Tokens.BitOr, Num (Int64.logand n 0x00000000FFFFFFFFL))]

let is_imm (a : atomic_exp) : bool =
  match a with
  | Ident _ -> false
  | Num _ -> true

let get_large_imm (a : atomic_exp) : int64 option =
  match a with
  | Num n ->
    let topmost = Int64.shift_right n 31 in
    if Int64.compare topmost 0L = 0 ||
       Int64.compare topmost 0xFFFFFFFFFFFFFFFFL = 0 then
      None
    else 
      Some n
  | _ -> None

let shrink_imm_elem (tmp_reg : var) (e : block_elem) : block_elem list =
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

let shrink_imm (cfg : cfg) : cfg =
  List.map 
    (fun cfg_entry ->
       { cfg_entry with elems = List.flatten (List.map (shrink_imm_elem (NamedTmp 0)) cfg_entry.elems) })
    cfg
