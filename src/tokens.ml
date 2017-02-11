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

open Util

(* Primitive operators *)
(* When extending with operators that can have side effects, be careful to
   check the rest of the compiler. Some optimisations can only be done on
   side-effect-free operations. Right now only Div can have an effect
   (divide-by-zero exception) *)

type op =
  (* integer operations *)
  | Plus
  | Minus
  | Times
  | Div
  | Lshift
  | BitOr
  | BitAnd
  (* comparisons *)
  | Lt
  | Gt
  | Eq
  (* boolean operations *)
  | And
  | Or

type uop =
  (* boolean negation *)
  | Not

let show_op op =
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

let show_uop (uop : uop) : string =
  match uop with
  | Not -> "!"

type token =
  | Num of int64
  | Ident of string (* identifiers represent various names, including function
                       and variable names *)
  | Op of op
  | Uop of uop
  (* Punctuation *)
  | Lparen
  | Rparen
  | Lcurly
  | Rcurly
  | Lbrac
  | Rbrac
  | Colon
  | Comma
  (* Identifier-like keywords *)
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

let show_token (t : token) : string =
  match t with
  | Num i -> Int64.to_string i
  | Ident s -> s
  | Op o -> show_op o
  | Uop u -> show_uop u
  | Lparen -> "("
  | Rparen -> ")"
  | Lcurly -> "{"
  | Rcurly -> "}"
  | Lbrac -> "["
  | Rbrac -> "]"
  | Comma -> ","
  | Colon -> ":"
  | While -> "while"
  | Do -> "do"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Assign -> ":="
  | True -> "true"
  | False -> "false"
  | Input -> "input"
  | Output -> "output"
  | Array -> "array"
  | Int -> "int"
  | Bool -> "bool"
  | Let -> "let"
  | Function -> "function"
  | Return -> "return"

(* Pretty-print a token *)
let pp_token (fmt : Format.formatter) (t : token) =
  Format.fprintf fmt "%s" (show_token t)

(* Tokens annotated with which line number they appear on *)
type tok_loc = (token * int)

(* Pretty-print a token and location *)
let pp_tok_loc (fmt : Format.formatter) ((t, l) : tok_loc) =
  Format.fprintf fmt "(%a, %d)" pp_token t l

(* The mapping of keword strings to the corresponding tokens *)
let keywords : (string * token) list =
  (* Derive the mapping from the to_string functions to avoid duplication *)
  (show_uop Not, Uop Not) ::
  List.map (fun o -> (show_op o, Op o))
    [Plus; Minus; Times; Div; Lt; Gt; Eq; And; Or; Lshift; BitOr; BitAnd] @
  List.map (fun t -> (show_token t, t))
    [Do; While; If; Then; Else; Array; Assign; True; Input; Output; False;
     Lparen; Rparen; Lcurly; Rcurly; Lbrac; Rbrac; Int; Bool; Colon; Let;
     Return; Function; Comma]

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
let newline_re = Str.regexp "\n\\|\r\n"

(* Read all the tokens from s, using pos to index into the string and line_n
   to track the current line number, for error reporting later on. Return them
   in a list. *)
(* The lex function repeatedly finds which regexp matches the string, starting
   from the current position, adds that token to the result, and then updates
   the starting position to find the next token *)
let rec lex (s : string) (pos : int) (line_n : int) : tok_loc list =
  if pos >= String.length s then
    []
  else if Str.string_match space_re s pos then
    lex s (Str.match_end ()) line_n
  else if Str.string_match newline_re s pos then
    lex s (Str.match_end ()) (line_n + 1)
  else if Str.string_match ident_re s pos then
    let id = Str.matched_string s in
    if Strmap.mem id keyword_map then
      (* The identifier is also a keyword *)
      let tok = Strmap.find id keyword_map in
      (tok, line_n) :: lex s (Str.match_end ()) line_n
    else
      (Ident id, line_n) :: lex s (Str.match_end ()) line_n
  else if Str.string_match keyword_re s pos then
    let tok = Strmap.find (Str.matched_string s) keyword_map in
    (tok, line_n) :: lex s (Str.match_end ()) line_n
  else if Str.string_match number_re s pos then
    let num =
      try Int64.of_string (Str.matched_string s)
      with Failure _ ->
        raise (BadInput ("Lex error: integer constant too big " ^
                         Str.matched_string s ^
                         " on line " ^
                         string_of_int line_n))
    in
    (Num num, line_n) :: lex s (Str.match_end ()) line_n
  else
    raise (BadInput ("lex error: at character '" ^
                     String.sub s pos 1 ^
                     "' on line " ^
                     string_of_int line_n))
