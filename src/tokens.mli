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

(* The language's tokens, and a simple lexer *)

type op =
  | Plus
  | Minus
  | Times
  | Div
  | Lshift
  | BitOr
  | BitAnd
  | Lt
  | Gt
  | Eq
  | And
  | Or

type uop =
  | Not

type token =
  | Num of int64
  | Ident of string
  | Op of op
  | Uop of uop
  | Lparen
  | Rparen
  | Lcurly
  | Rcurly
  | Lbrac
  | Rbrac
  | Colon
  | Comma
  | While
  | Do
  | If
  | Then
  | Else
  | Assign
  | True
  | False
  | Input
  | Output
  | Array
  | Int
  | Bool
  | Let
  | Function
  | Return

type tok_loc = (token * int)

val show_uop : uop -> string
val show_op : op -> string
val show_token : token -> string
val pp_tok_loc : Format.formatter -> tok_loc -> unit
val lex : string -> int -> int -> tok_loc list
