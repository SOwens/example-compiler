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

(* The language's tokens, and a simple lexer *)

open Util

(* Primitive operators *)
(* When extending with impure operators, be careful to check the rest of the
   compiler where it depends on purity. Right now only Div is impure
   (divide-by-zero) *)

type op =
  | Plus
  | Minus
  | Times
  | Div
  | Lt
  | Gt
  | Eq
  | And
  | Or
  | Lshift
  | BitOr
  | BitAnd
  [@@deriving show]

type uop =
  | Not
  [@@deriving show]

let op_to_string op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "="
  | And -> "&&"
  | Or -> "||"
  | Lshift -> "<<"
  | BitOr -> "|"
  | BitAnd -> "&"

let uop_to_string uop =
  match uop with
  | Not -> "!"

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
  [@@deriving show]

type tok_loc = (token * int)
  [@@ deriving show]

let keywords =
  [("do", Do); ("while",While); ("if",If); ("then",Then); ("else",Else);
   ("array",Array); (":=",Assign); ("true",True); ("input", Input);
   ("output",Output); ("false",False); ("(",Lparen); (")",Rparen);
   ("{",Lcurly); ("}",Rcurly); ("[",Lbrac); ("]",Rbrac);

   (* Derive the mapping from the to_string functions to avoid duplication *)
   (uop_to_string Not, Uop Not)] @
  List.map (fun o -> (op_to_string o, Op o))
    [Plus; Minus; Times; Div; Lt; Gt; Eq; And; Or; Lshift; BitOr; BitAnd]

(* Map each keyword string to its corresponding token *)
let keyword_map : token Strmap.t =
  List.fold_left (fun m (k,v) -> Strmap.add k v m) Strmap.empty keywords

(* Regular expressions that describe various syntactic entities *)
(* Build the keyword regexp out of the mapping to avoid duplication *)
let keyword_re =
  Str.regexp
    (String.concat "\\|"
       (List.map (fun (s, _) -> Str.quote s) keywords))
let number_re = Str.regexp "[0-9]+"
let ident_re = Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*"
let space_re = Str.regexp "[ \t]+\\|//.*"
let newline_re = Str.regexp "\n"

(* Read all the tokens from s, using pos to index into the string and line_n
   to track the current line number, for error reporting later on. Return them
   in a list. *)
let rec lex (s : string) (pos : int) (line_n : int) : tok_loc list =
  if pos >= String.length s then
    []
  else if Str.string_match space_re s pos then
    lex s (Str.match_end ()) line_n
  else if Str.string_match newline_re s pos then
    lex s (Str.match_end ()) (line_n + 1)
  else if Str.string_match keyword_re s pos then
    let tok = Strmap.find (Str.matched_string s) keyword_map in
    (tok, line_n) :: lex s (Str.match_end ()) line_n
  else if Str.string_match ident_re s pos then
    (* Need the let because of OCaml's right-to-left evaluation *)
    let id = Str.matched_string s in
    (Ident id, line_n) :: lex s (Str.match_end ()) line_n
  else if Str.string_match number_re s pos then
    let num =
      try Int64.of_string (Str.matched_string s)
      with Failure _ ->
        raise (BadInput ("Integer constant too big " ^
                         Str.matched_string s ^
                         " on line " ^
                         string_of_int line_n))
    in
    (Num num, line_n) :: lex s (Str.match_end ()) line_n
  else
    raise (BadInput ("At character '" ^
                     String.sub s pos 1 ^
                     "' on line " ^
                     string_of_int line_n))
