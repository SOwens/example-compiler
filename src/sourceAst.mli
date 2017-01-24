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

(* The language's AST, and a predictive, recursive descent parser. See the
   ../README.md for the grammar. *)

type id =
  | Source of string
  | Temp of string * int

module Idmap : Map.S with type key = id

type exp =
  | Ident of id * exp list
  | Num of int64
  | Bool of bool
  | Op of exp * Tokens.op * exp
  | Uop of Tokens.uop * exp
  (* Allocate a new array of given dimensions. Initialise to 0 *)
  | Array of exp list

type stmt =
  | Assign of id * exp list * exp
  (* A generalised do/while loop. Always execute the first statement, then
     the test, then repeatedly do the 2nd, then first statement and then test
     'while e s' becomes DoWhile (Stmts [], e, s) and 'do s while e' becomes
     DoWhile (s, e, Stmts []) *)
  | DoWhile of stmt * exp * stmt
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | In of id
  | Out of id
  | Loc of stmt * int (* annotate a statement with it's source line number *)

val show_id : id -> string
val pp_stmt : Format.formatter -> stmt -> unit
val pp_stmts : Format.formatter -> stmt list -> unit

val parse_program : (Tokens.token * int) list -> stmt list
val stmts_to_stmt : stmt list -> stmt
