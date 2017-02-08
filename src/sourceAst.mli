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

(* The language's AST, and a predictive, recursive descent parser. See the
   ../README.md for the grammar. *)

type scope =
  | Global
  | Parameter
  | Local

val compare_scope : scope -> scope -> int

type id =
  | Source of string * scope option
  | Temp of string * int

module Idmap : Map.S with type key = id

type exp =
  | Ident of id * exp list
  | Call of id * exp list
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
  | Return of id option
  | Loc of stmt * int (* annotate a statement with it's source line number *)

type typ =
  | Int
  | Bool
  (* An int array with the given number of dimensions *)
  | Array of int

type var_dec = { var_name : id; typ : typ; init : exp; loc : int option }

type func = { fun_name : id; params : (id * typ) list; ret : typ;
              locals : var_dec list; body : stmt list; loc : int option }

type prog = { globals : var_dec list; funcs : func list }

val show_id : id -> string
val pp_stmt : Format.formatter -> stmt -> unit
val pp_stmts : Format.formatter -> stmt list -> unit
val pp_program : Format.formatter -> prog -> unit
val parse_program : (Tokens.token * int) list -> prog
val stmts_to_stmt : stmt list -> stmt
