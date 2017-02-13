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

(* The main driver for the compiler executable. *)

(* Command-line arguments *)
let filename_ref = ref None;;
let safe_ref = ref true;;

let options = Arg.align ([("-safe", Arg.Bool (fun b -> safe_ref := b),
                           "\tdo null pointer and array bounds checks")
 ]);;

let usage_msg =
  "example compiler \nexample usage:    " ^ Sys.argv.(0)^ " test.expl\n"

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

(* Build a main function that just runs the initialisation of all of the
   globals *)
let main_function =
  { fun_name = Source ("main", None); params = []; ret = Int;
    locals = [];
    body = List.map (fun d -> Assign (d.var_name, [], d.init)) prog.globals @
           [Return None];
    loc = None }

let global_id_to_var (g : SourceAst.id) : BlockStructure.var =
  match g with
  | Source (i, Some Global) -> BlockStructure.NamedSource (i, Global)
  | _ -> assert false

let globals =
  List.fold_right
    (fun d globals ->
       BlockStructure.Varset.add (global_id_to_var d.var_name) globals)
    prog.globals
    BlockStructure.Varset.empty ;;

let functions =
  List.map (CompileFunction.compile_fun !safe_ref filename globals)
    (main_function::prog.funcs);;

open Format

let outfile = open_out (Filename.chop_extension filename ^ ".s");;
let fmt = formatter_of_out_channel outfile;;
(* Assembly wrapper *)
fprintf fmt "[section .text align=16]@\n";;
fprintf fmt "global main@\n@\n";;
fprintf fmt "extern signal_error@\n";;
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
fprintf fmt "bound_error:@\n%a"
  (fun fmt instr -> X86.pp_instr_list fmt (InstrSelX86.be_to_x86 !safe_ref instr))
  (BlockStructure.Call (None, "signal_error", [BlockStructure.Num 0L]));;
fprintf fmt "null_error:@\n%a"
  (fun fmt instr -> X86.pp_instr_list fmt (InstrSelX86.be_to_x86 !safe_ref instr))
  (BlockStructure.Call (None, "signal_error", [BlockStructure.Num 1L]));;
(* bss segment for the global variables, all initialised to 0 *)
fprintf fmt "[section .bss align=16]@\n";;
fprintf fmt "default rel@\n";;
List.iter (fun d -> fprintf fmt "%s: resq 1\n" (show_id d.var_name)) prog.globals;;
close_out outfile;;
