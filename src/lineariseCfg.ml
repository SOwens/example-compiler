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

open BlockStructure

type linear =
  | Instr of block_elem
  | CJump of test * bool * string (* jump to string if var is bool *)
  | Jump of string
  | Label of string
  [@@deriving show]

let pp_linear fmt l =
  match l with
  | Instr b ->
    Format.fprintf fmt "  %a@\n" pp_block_elem b
  | CJump (v, b, s) ->
    Format.fprintf fmt "  if %a = %s goto %s@\n"
      pp_test v
      (if b then "true" else "false")
      s
  | Jump s ->
    Format.fprintf fmt "  goto %s@\n" s
  | Label s ->
    Format.fprintf fmt "%s:@\n" s

type linear_list = linear list
  [@@deriving show]

let rec pp_linear_list fmt ls =
  match ls with
  | [] -> ()
  | x::y ->
    (pp_linear fmt x;
     pp_linear_list fmt y)

module I = Map.Make(struct type t = int let (compare : int -> int -> int) = compare end)

(* Initialise the mutable visited fields to false, and put CFG into a map *)
let init_traversal (cfg : cfg) : cfg_entry I.t =
  List.iter (fun x -> x.started <- false; x.finished <- false) cfg;
  List.fold_left (fun map x -> I.add x.bnum x map) I.empty cfg


(* Linearise the reachable cfg, starting from the block with index next_block,
   but don't do already visited blocks *)
(* This is essentially a DFS, pre-order traversal *)
let rec cfg_to_linear (next_block : int) (cfg : cfg_entry I.t) : linear list =
  let b = I.find next_block cfg in
  if b.finished then
    (* Don't output the block twice *)
    []
  else
    (b.finished <- true;
     Label ("block" ^ string_of_int b.bnum) ::
     List.map (fun x -> Instr x) b.elems @
     match b.next with
     | End ->
       [Jump "exit"]
     | Next i ->
       if (I.find i cfg).started then
         (* We've started the next block, so we'll just jump to it *)
         [Jump ("block" ^ string_of_int i)]
       else
         (* We haven't started the next block, so we can put it here and omit the jump *)
         ((I.find i cfg).started <- true;
          cfg_to_linear i cfg)
     | Branch (v, t1, t2) ->
       let c1 = I.find t1 cfg in
       let c2 = I.find t2 cfg in
       match (c1.started, c2.started) with
       | (false, false) ->
         c1.started <- true;
         c2.started <- true;
         CJump (v, true, "block"  ^ string_of_int t1) ::
         cfg_to_linear t2 cfg @
         cfg_to_linear t1 cfg
       | (true, true) ->
         [CJump (v, true, "block"  ^ string_of_int t1);
          Jump ("block" ^ string_of_int t2)]
       | (true, false) ->
         (c2.started <- true;
          [CJump (v, true, "block"  ^ string_of_int t1)] @
          cfg_to_linear t2 cfg)
       | (false, true) ->
         (c1.started <- true;
          [CJump (v, false, "block"  ^ string_of_int t2)] @
          cfg_to_linear t1 cfg))

let cfg_to_linear cfg =
  cfg_to_linear 1 (init_traversal cfg)
