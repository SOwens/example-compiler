open Util

let ast = FrontEnd.front_end true;;

let (_,opt_ast) = ConstProp.prop_stmts Strmap.empty ast;;
print_newline ();;
print_string ([%show: SourceAst.stmt list] opt_ast);;
print_newline ();;

let cfg = BlockStructure.build_cfg opt_ast;;
print_newline ();;
print_string ([%show: SourceAst.id BlockStructure.cfg] cfg);;
print_newline ();;

let lva_cfg = LiveVarAnalysis.lva cfg;;
print_newline ();;
print_string (LiveVarAnalysis.show_cfg lva_cfg);;
print_newline ();;

let lva_cfg' = LiveVarAnalysis.remove_unused_writes lva_cfg;;
print_newline ();;
print_string (LiveVarAnalysis.show_cfg lva_cfg');;
print_newline ();;

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

let lva_cfg'' = LiveVarAnalysis.lva (List.map fst lva_cfg');;
print_newline ();;
print_string (LiveVarAnalysis.show_cfg lva_cfg'');;
print_newline ();;

let lva_cfg''' = LiveVarAnalysis.remove_unused_writes lva_cfg'';;
print_newline ();;
print_string (LiveVarAnalysis.show_cfg lva_cfg''');;
print_newline ();;
