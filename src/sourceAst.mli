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

module T = Tokens 
type id = string [@@deriving show]

type exp = 
  (* the int is the line number of the identifier *)
  | Ident of id * int
  | Num of Int64.t
  | Bool of bool
  (* the int is the line number of the operator *)
  | Oper of exp * (T.op * int) * exp
              [@@deriving show]

(* the ints are all the line number of the (start of) the stmt *)
type stmt = 
  | Assign of id * exp * int
  | While of exp * stmt * int
  | Ite of exp * stmt * stmt * int
  | Stmts of stmt list * int
  | In of id * int
  | Out of id * int
             [@@deriving show]

val parse_program : (Tokens.token * int) list -> stmt list
