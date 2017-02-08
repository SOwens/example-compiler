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

(* An interpreter for ASTs *)

(* For when the interpreter crashed, such as array bounds violations *)
exception Crash of string

(* For errors that a well-typed program can't have *)
exception TypeError

(* Values are either integers or n-dimensional arrays of integers.
   We keep multi-dimensional arrays in a single dimensional one and include a
   list of how big each dimension is.
   We represent bools as numbers: true = 1L and false = 0L *)
type val_t =
  | Vint of int64
  | Varray of int list * int64 array

val do_op : Tokens.op -> int64 -> int64 -> int64

val interp_prog : SourceAst.prog -> unit
