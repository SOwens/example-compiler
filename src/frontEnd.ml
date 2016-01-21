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

open Util

let front_end (filename : string) (print_intermediates : bool) : SourceAst.stmt list =
  let p s =
    if print_intermediates then
      Printf.printf "%s\n" s
    else ()
  in
  if Filename.check_suffix filename ".expl" then
    let input = Std.input_file filename in
    let toks = Tokens.lex input 0 1 in
    p ([%show: Tokens.tok_loc list] toks);
    let ast = SourceAst.parse_program toks in
    p ([%show: SourceAst.stmt list] ast);
    TypeCheck.type_stmts Strmap.empty ast;
    ast
  else
    (Format.printf "Expects filename ending in .expl\n";
     exit 1)
