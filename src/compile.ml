open Util
open Format

let (filename, ast) = FrontEnd.front_end true;;

let (_,opt_ast) = ConstProp.prop_stmts Strmap.empty ast;;
print_newline ();;
print_string ([%show: SourceAst.stmt list] opt_ast);;
print_newline ();;

let no_bool_ast = RemoveBool.remove_and_or opt_ast;;
print_newline ();;
print_string ([%show: SourceAst.stmt list] no_bool_ast);;
print_newline ();;

let cfg = BlockStructure.build_cfg no_bool_ast;;
printf "@\n%a@\n" BlockStructure.pp_cfg cfg;;

let cfg' = ShrinkImmediates.shrink_imm cfg;;
printf "@\n%a@\n" BlockStructure.pp_cfg cfg';;

let lva_cfg0 = LiveVarAnalysis.lva cfg';;
printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg0;;

let lva_cfg1 = LiveVarAnalysis.remove_unused_writes lva_cfg0;;
printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg1;;

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
printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg2;;

let lva_cfg3 = LiveVarAnalysis.remove_unused_writes lva_cfg2;;
printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg3;;

let lva_cfg4 = LiveVarAnalysis.lva (List.map fst lva_cfg3);;
printf "@\n%a@\n" LiveVarAnalysis.pp_cfg lva_cfg4;;

let reg_cfg = RegAlloc.reg_alloc 0 (List.map fst lva_cfg4);;
printf "@\n%a@\n" BlockStructure.pp_cfg reg_cfg;;

let linear = LineariseCfg.cfg_to_linear reg_cfg;;
printf "@\n%a@\n" LineariseCfg.pp_linear_list linear;;

let x86 = InstrSelX86.to_x86 linear;;
let outfile = open_out (Filename.chop_extension filename ^ ".s");;
let fmt = formatter_of_out_channel outfile;;
fprintf fmt "[section .text align=16]@\n";;
fprintf fmt "global start@\n@\n";;
fprintf fmt "start:@\n";;
fprintf fmt "%a" X86.pp_instr_list x86;;
(* Prepare for exit system call *)
fprintf fmt "exit:";; 
fprintf fmt "  mov RAX, 0x2000001@\n";; (* Do syscall 1 *)
fprintf fmt "  mov RDI, 0@\n";;         (* Exit with 0, e.g. success *)
fprintf fmt "  syscall@\n@\n";;
(* OS X crashes if there isn't a data segment with something in it *)
fprintf fmt "[section .data align=16]@\n";;
fprintf fmt "dummy: db \" \", 0x0a";;
close_out outfile;;
