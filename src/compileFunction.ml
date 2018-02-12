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

(* Compiler for a single function *)

open SourceAst

(* We treat a local variable declarations as simple assignment to the variable.
*)
let init_var_dec_to_0 (d : var_dec) : stmt =
  Assign (d.var_name, [], Num 0L)

let var_dec_to_stmt (d : var_dec) : stmt =
  Assign (d.var_name, [], d.init)

(* globals should contain all globals variables, so that we can tell the live
   variable analysis that they must be live when the function returns.
*)
let compile_fun safe filename (globals : BlockStructure.Varset.t) (f : func)
  : id * X86.instruction list =
  let ast =
    (* Zero out local variables, in case any of the initialisations are out of
       order and refer to uninitialised variables. Later removal of dead writes
       should be able to remove these where there is no problem *)
    List.map init_var_dec_to_0 f.locals @
    List.map var_dec_to_stmt f.locals @
    f.body in

  let (_,opt_ast) = ConstProp.prop_stmts SourceAst.Idmap.empty ast in
   (*Format.printf "@\n%a@\n" SourceAst.pp_stmts opt_ast; *)

  let no_nest_ast = UnnestExp.unnest opt_ast in
  (*Format.printf "@\n%a@\n" SourceAst.pp_stmts no_nest_ast; *)

  let cfg = BlockStructure.build_cfg no_nest_ast in
   (*Format.printf "@\n%a@\n" BlockStructure.pp_cfg cfg; *)
  (* Print the CFG in dot format. Process the .dot file with dot -Tpdf FILENAME > FILENAME.pdf.
     dot is part of the graphviz package http://www.graphviz.org *)
  (*
  let outfile = open_out (Filename.chop_extension filename ^ "_" ^ show_id f.fun_name ^ ".dot") in
  let fmt = Format.formatter_of_out_channel outfile in
  Format.fprintf fmt "%a" BlockStructure.cfg_to_graphviz cfg;
  close_out outfile;
     *)

  let cfg' = ShrinkImmediates.shrink_imm cfg in
  (* Format.printf "@\n%a@\n" BlockStructure.pp_cfg cfg';*)

  let lva_cfg0 = LiveVarAnalysis.lva globals cfg' in
  (* Format.printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg0; *)

  let lva_cfg1 = LiveVarAnalysis.remove_unused_writes lva_cfg0 in
  (*Format.printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg1;*)


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

  let lva_cfg2 = LiveVarAnalysis.lva globals (List.map fst lva_cfg1) in
  (* Format.printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg2; *)

  let lva_cfg3 = LiveVarAnalysis.remove_unused_writes lva_cfg2 in
  (* Format.printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg3; *)

  let lva_cfg4 = LiveVarAnalysis.lva globals (List.map fst lva_cfg3) in
  (* Format.printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg4;*)

  let (reg_cfg, num_stack) =
    RegAlloc.reg_alloc (Util.zip (List.map fst f.params) InstrSelX86.argument_reg_numbers)
      InstrSelX86.num_regs
      (List.map fst lva_cfg4)
  in
   (* Format.printf "@\n%a@\n" BlockStructure.pp_cfg reg_cfg; *)

  let linear = LineariseCfg.cfg_to_linear reg_cfg in
  Format.printf "@\n%a@\n" LineariseCfg.pp_linear_list linear;

  let x86 = InstrSelX86.to_x86 safe linear num_stack in

  (f.fun_name, x86)
