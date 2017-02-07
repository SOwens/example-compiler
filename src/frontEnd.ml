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

(* The front end packaging lexing, parsing and type checking *)

open Util

module Std = struct
  (* Copy and paste from extlib to get input_file without making a dependency.
     extlib is LGPL 2.1, and so this sub-module is too.
     https://github.com/ygrek/ocaml-extlib/blob/33f744ddb28d6a0f4c96832145e1a6e384644709/src/std.ml *)

  let finally handler f x =
    let r = (
      try
        f x
      with
        e -> handler (); raise e
    ) in
    handler ();
    r

  let buf_len = 8192

  let input_all ic =
    let rec loop acc total buf ofs =
      let n = input ic buf ofs (buf_len - ofs) in
      if n = 0 then
        let res = Bytes.create total in
        let pos = total - ofs in
        let _ = Bytes.blit buf 0 res pos ofs in
        let coll pos buf =
          let new_pos = pos - buf_len in
          Bytes.blit buf 0 res new_pos buf_len;
          new_pos in
        let _ = List.fold_left coll pos acc in
        (* [res] doesn't escape and will not be mutated again *)
        Bytes.unsafe_to_string res
      else
        let new_ofs = ofs + n in
        let new_total = total + n in
        if new_ofs = buf_len then
          loop (buf :: acc) new_total (Bytes.create buf_len) 0
        else loop acc new_total buf new_ofs in
    loop [] 0 (Bytes.create buf_len) 0

  let input_file ?(bin=false) fname =
    let ch = (if bin then open_in_bin else open_in) fname in
    finally (fun () -> close_in ch) input_all ch

end

let front_end (filename : string) (print_intermediates : bool) : SourceAst.prog
  =
  if Filename.check_suffix filename ".expl" then
    let input = Std.input_file filename in
    let toks = Tokens.lex input 0 1 in
    if print_intermediates then
      Format.printf "%a@\n@\n" (pp_list Tokens.pp_tok_loc) toks
    else
      ();
    let ast = SourceAst.parse_program toks in
    if print_intermediates then
      Format.printf "%a@\n@\n" SourceAst.pp_program ast
    else
      ();
    let ast2 = TypeCheck.type_prog ast in
    if print_intermediates then
      Format.printf "%a@\n@\n" SourceAst.pp_program ast2
    else
      ();
    ast2
  else
    (Format.printf "Expects filename ending in .expl\n";
     exit 1)
