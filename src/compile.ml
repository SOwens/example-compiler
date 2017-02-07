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

(* The main driver for the compiler *)

open Util
open Format

(* Command-line arguments *)
let filename_ref = ref None;;

let options = Arg.align ([
 ]);;

let usage_msg =
  "example compiler \nexample usage:       compile.byte test.expl\n"

let _ =
  Arg.parse options
    (fun s ->
       match !filename_ref with
       | None ->
         filename_ref := Some s
       | Some s' ->
         (Format.printf "Error: given multiple files to process: %s and %s\n"
            s' s;
          exit 1))
    usage_msg

let filename =
 match !filename_ref with
  | None ->
    (print_string usage_msg;
     exit 1)
  | Some filename ->
    filename

let prog = FrontEnd.front_end filename false;;

open SourceAst

let functions = List.map CompileFunction.compile_fun prog.funcs

let outfile = open_out (Filename.chop_extension filename ^ ".s");;
let fmt = formatter_of_out_channel outfile;;
(* Assembly wrapper *)
fprintf fmt "[section .text align=16]@\n";;
fprintf fmt "global main@\n@\n";;
fprintf fmt "extern input@\n";;
fprintf fmt "extern output@\n";;
fprintf fmt "extern allocate1@\n";;
fprintf fmt "extern allocate2@\n";;
fprintf fmt "extern allocate3@\n";;
fprintf fmt "extern allocate4@\n";;
fprintf fmt "extern allocate5@\n";;
fprintf fmt "extern allocate6@\n";;
fprintf fmt "extern allocate7@\n@\n";;
List.iter
  (fun (name, code) -> fprintf fmt "%s:@\n%a" (show_id name) X86.pp_instr_list code)
  functions;;
fprintf fmt "main:@\n";;
(*fprintf fmt "%a" X86.pp_instr_list x86;;*)
(* Prepare for exit system call *)
fprintf fmt "@\nexit:@\n";;
fprintf fmt "  mov rax, 0@\n";; (* Exit with 0, e.g. success *)
fprintf fmt "  leave@\n";;
fprintf fmt "  ret@\n@\n";;
fprintf fmt "bound_error:@\n";;
fprintf fmt "  mov rax, 1@\n";; (* Exit with 1, e.g. failure *)
fprintf fmt "  leave@\n";;
fprintf fmt "  ret@\n@\n";;
(* OS X crashes if there isn't a data segment with something in it *)
fprintf fmt "[section .data align=16]@\n";;
fprintf fmt "dummy: db \" \", 0x0a";;
close_out outfile;;
