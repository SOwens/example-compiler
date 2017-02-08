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

(* Flatten the CFG into a list of three-address code. *)

open BlockStructure
type linear =
  | Instr of block_elem
  | CJump of test * bool * string (* jump to string if var is bool *)
  | Jump of string
  | Label of string
  | Return of var option

type linear_list = linear list

val cfg_to_linear : cfg -> linear list

val pp_linear_list : Format.formatter -> linear list -> unit

val init_traversal : cfg -> cfg_entry Util.Intmap.t
