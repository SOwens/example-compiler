open Util

let front_end (print_intermediates : bool) : SourceAst.stmt list =
  let p s =
    if print_intermediates then
      Printf.printf "%s\n" s
    else ()
  in
  assert (Array.length Sys.argv = 2);
  let filename = Sys.argv.(1) in
  let input = Std.input_file filename in
  let toks = Tokens.lex input 0 1 in
  p ([%show: Tokens.tok_loc list] toks);
  let ast = SourceAst.parse_program toks in
  p ([%show: SourceAst.stmt list] ast);
  TypeCheck.type_stmts Strmap.empty ast;
  ast
