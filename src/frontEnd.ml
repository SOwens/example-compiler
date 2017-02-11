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

(* The front end. It packages lexing, parsing and type checking for use by the
   interpreter and type checker both. *)

open Util

(* Parse and type check the given filename. *)
let front_end (filename : string) (debug : bool) : SourceAst.prog
  =
  if Filename.check_suffix filename ".expl" then
    try
      let input = Std.input_file filename in
      let toks = Tokens.lex input 0 1 in
      if debug then
        Format.printf "%a@\n@\n" (pp_list Tokens.pp_tok_loc) toks;
      let ast = SourceAst.parse_program toks in
      if debug then
        Format.printf "%a@\n@\n" SourceAst.pp_program ast;
      let ast2 = TypeCheck.type_prog ast in
      if debug then
        Format.printf "%a@\n@\n" SourceAst.pp_program ast2;
      ast2
    with
    | BadInput s ->
      Format.printf "%s\n" s;
      exit 1
  else
    (Format.printf "Expects filename ending in .expl\n";
     exit 1)
