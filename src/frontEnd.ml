open Util

exception CommandLine

let front_end (print_intermediates : bool) : (string * SourceAst.stmt list) =
  let p s =
    if print_intermediates then
      Printf.printf "%s\n" s
    else ()
  in
  (if Array.length Sys.argv <> 2 then
     (Format.printf "Expects exactly one argument\n";
      exit 1)
   else
     ());
  let filename = Sys.argv.(1) in
  if Filename.check_suffix filename ".expl" then
    let input = Std.input_file filename in
    let toks = Tokens.lex input 0 1 in
    p ([%show: Tokens.tok_loc list] toks);
    let ast = SourceAst.parse_program toks in
    p ([%show: SourceAst.stmt list] ast);
    TypeCheck.type_stmts Strmap.empty ast;
    (filename, ast)
  else
    (Format.printf "Expects filename ending in .expl\n";
     exit 1)
