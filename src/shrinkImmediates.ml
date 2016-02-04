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

let tmp_var = NamedTmp ("SI",0)
let tmp_var2 = NamedTmp ("SI",1)

(* Build a series of assignments that puts the immediate n into the dest
   register, using only immediates of 32 bits or smaller *)
let assign_imm (dest : var) (n : int64) : block_elem list =
  [AssignAtom (dest, Num (Int64.shift_right_logical n 32));
   AssignOp (dest, Ident dest, Tokens.Lshift, Num 32L);
   AssignOp (dest, Ident dest, Tokens.BitOr,
             Num (Int64.logand n 0x00000000FFFFFFFFL))]

let is_imm (a : atomic_exp) : bool =
  match a with
  | Ident _ -> false
  | Num _ -> true

(* If the atomic_exp is a large immediate constant (takes more thatn 32 bits),
   then return the int64, else None. Relies on int64 being 2s complement. It
   it's negative (the top bit is 1), then check that the top 33 bits are 1. It
   it's non-negative, check that the top 33 bits are 0. 32 won't suffice,
   because when we truncate to 32 bits only, the top bit needs to be the sign
   bit. *)
let get_large_imm (a : atomic_exp) : int64 option =
  match a with
  | Num n ->
    (* This is an arithmetic shift. *)
    let topmost = Int64.shift_right n 31 in
    if Int64.compare topmost 0L = 0 ||
       Int64.compare topmost 0xFFFFFFFFFFFFFFFFL = 0 then
      None
    else
      Some n
  | _ -> None

let shrink_imm_elem (e : block_elem) : block_elem list =
  match e with
  | AssignOp (dest, a1, op, a2) ->
    assert (not (is_imm a1 && is_imm a2));
    begin
      match get_large_imm a1 with
      | Some n ->
        assign_imm tmp_var n @
        [AssignOp (dest, Ident tmp_var, op, a2)]
      | None ->
        begin
          match get_large_imm a2 with
          | Some n ->
            assign_imm tmp_var n @
            [AssignOp (dest, a1, op, Ident tmp_var)]
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
  | Ld (v1, v2, a) ->
    begin
      match get_large_imm a with
      | Some n ->
        assign_imm tmp_var n @ [Ld(v1, v2, Ident tmp_var)]
      | None -> [e]
    end
  | St (r, a1, a2) ->
    begin
      match (get_large_imm a1, get_large_imm a2) with
      | (None, None) -> [e]
      | (Some n1, None) ->
        assign_imm tmp_var n1 @ [St(r, Ident tmp_var, a2)]
      | (None, Some n2) ->
        assign_imm tmp_var n2 @ [St(r, a1, Ident tmp_var)]
      | (Some n1, Some n2) ->
        assign_imm tmp_var n1 @
        assign_imm tmp_var2 n2 @
        [St(r, Ident tmp_var, Ident tmp_var2)]
    end
  | Call (v, f, aes) ->
    let (s, es, _) =
      List.fold_right
        (fun (ae : atomic_exp) ((s : block_elem list), es, n) ->
           match get_large_imm ae with
           | None -> (s, ae::es, n)
           | Some imm ->
             (assign_imm (NamedTmp ("SI",n)) imm @ s, Ident tmp_var::es, n + 1))
        aes
        ([], [], 2)
    in
    s @ [Call (v, f, es)]

let shrink_imm (cfg : cfg) : cfg =
  List.map
    (fun cfg_entry ->
       { cfg_entry with
         elems =
           List.flatten
             (List.map shrink_imm_elem cfg_entry.elems) })
    cfg
