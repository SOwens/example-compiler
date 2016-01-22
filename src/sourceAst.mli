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

(* The langauage's tokens, and a simple lexer *)

module T = Tokens
type id = string [@@deriving show]

type exp =
  | Ident of id * exp list
  | Num of Int64.t
  | Bool of bool
  | Oper of exp * T.op * exp
  (* Allocate a new array of given dimensions. Initialise to 0 *)
  | Array of exp list
  [@@deriving show]

type stmt =
  | Assign of id * exp list * exp
  | While of exp * stmt
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | In of id
  | Out of id
  | Loc of stmt * int (* annotate a statement with it's source line number *)
  [@@deriving show]

val parse_program : (Tokens.token * int) list -> stmt list
