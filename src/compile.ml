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

let ast = FrontEnd.front_end filename true;;

let (_,opt_ast) = ConstProp.prop_stmts SourceAst.Idmap.empty ast;;
 (* printf "@\n%a@\n" SourceAst.pp_stmts opt_ast;; *)

let no_nest_ast = UnnestExp.unnest opt_ast;;
(* printf "@\n%a@\n" SourceAst.pp_stmts no_nest_ast;; *)

let cfg = BlockStructure.build_cfg no_nest_ast;;
(* printf "@\n%a@\n" BlockStructure.pp_cfg cfg;; *)
(* Print the CFG in dot format. Process the .dot file with dot -Tpdf FILENAME > FILENAME.pdf.
   dot is part of the graphviz package http://www.graphviz.org *)
(*
let outfile = open_out (Filename.chop_extension filename ^ ".dot") in
let fmt = formatter_of_out_channel outfile in
fprintf fmt "%a" BlockStructure.cfg_to_graphviz cfg;
close_out outfile;;
   *)

let cfg' = ShrinkImmediates.shrink_imm cfg;;
(* printf "@\n%a@\n" BlockStructure.pp_cfg cfg';; *)

let lva_cfg0 = LiveVarAnalysis.lva cfg';;
(* printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg0;; *)

let lva_cfg1 = LiveVarAnalysis.remove_unused_writes lva_cfg0;;
(* printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg1;; *)

(* Iterate analysis for examples like this:

input c
x := c
y := 1
if (c = 0) then
  y := x
else
  y := x
z := y
a := z
output x

*)

let lva_cfg2 = LiveVarAnalysis.lva (List.map fst lva_cfg1);;
(* printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg2;; *)

let lva_cfg3 = LiveVarAnalysis.remove_unused_writes lva_cfg2;;
(* printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg3;; *)

let lva_cfg4 = LiveVarAnalysis.lva (List.map fst lva_cfg3);;
(* printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg4;;*)

let (reg_cfg, num_stack) =
  RegAlloc.reg_alloc (* InstrSelX86.num_regs*) 0 (List.map fst lva_cfg4);;
(* printf "@\n%a@\n" BlockStructure.pp_cfg reg_cfg;; *)

let linear = LineariseCfg.cfg_to_linear reg_cfg;;
(* printf "@\n%a@\n" LineariseCfg.pp_linear_list linear;; *)

let x86 = InstrSelX86.to_x86 linear num_stack;;
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
fprintf fmt "main:@\n";;
fprintf fmt "%a" X86.pp_instr_list x86;;
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
