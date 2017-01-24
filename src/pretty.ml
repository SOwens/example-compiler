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
module T = Tokens 
open SourceAst  

let rec pretty_exp (e : exp) : string =
  match e with
  | Ident (id,_) -> id
  | Num n -> Int64.to_string n
  | Bool b -> string_of_bool b
  | Oper (e1,(op,_),e2) ->
    let pretty_op = 
      match op with 
      | T.Plus -> "+" 
      | T.Minus -> "-" 
      | T.Times -> "*" 
      | T.Div -> "/" 
      | T.Lt -> "<" 
      | T.Eq -> "=" 
      | T.And -> "&&" 
      | T.Or -> "||" 
      | T.Lshift -> "<<" 
      | T.BitOr -> "|" 
    in 
    "(" ^ pretty_exp e1 ^ pretty_op ^ pretty_exp e2 ^ ")" 

let test_print_exp1 =
  pretty_exp (Oper (Ident ("foo",23),(T.Times,99),(Num (Int64.of_int 345))))

let spaces (n : int) : string =
  String.make n ' '

let rec pretty_stmts  (ind : int) (stmts : stmt list) : string =
  List.fold_right (fun s t -> s^t) (List.map (pretty_stmt ind) stmts) ""
and
  pretty_stmt (ind : int) (st : stmt) : string =
  match st with
  | Assign (id,exp,_) -> 
    spaces ind ^ id ^ " := " ^ pretty_exp exp ^ "\n"
  | While (exp,stmt,_) -> 
    spaces ind ^ "while " ^ pretty_exp exp ^ "\n" ^ 
      pretty_stmt (ind+2) stmt
  | Ite (exp,stmt1,stmt2,_) -> 
    spaces ind ^ "if " ^ pretty_exp exp ^ "\n" ^ 
      spaces (ind+2) ^ "then\n" ^ 
        pretty_stmt (ind+4) stmt1 ^ 
      spaces (ind+2) ^ "else\n" ^ 
        pretty_stmt (ind+4) stmt2
  | Stmts (stmts,_) ->
    spaces ind ^ "{\n" ^
    String.concat "" (List.map (pretty_stmt (ind+2)) stmts) ^
    spaces ind ^ "}\n"
  | In (id,_) -> spaces ind ^ "input " ^ id ^ "\n"
  | Out (id,_) -> spaces ind ^ "output " ^ id ^ "\n"

(*
This is a string that is evaluated lazily, when applied to a null value ()
Doing this means that the effect can be seen in the intereactive
session, rather than on loading the file.
   
*) 

let stmt1 =
  Assign ("x",(Oper (Ident ("foo",23),(T.Times,99),(  Num (Int64.of_int 345)))),345)

let stmt2 =
  let exp = Oper (Ident ("foo",23),(T.Lt,99),(  Num (Int64.of_int 345))) in
  let stmt = Assign ("y",(Oper (Ident ("bar",23),(T.Times,99),(  Num (Int64.of_int 45)))),345) in
  Ite (exp, stmt, stmt1, 0)

let stmts1 = [stmt1;stmt2;stmt1]

let stmt3 = While (Oper (Ident ("foo",23),(T.Lt,99),(  Num (Int64.of_int 345))),
                   Stmts (stmts1,999),999)

let test_print_stmts1 =
  fun () -> 
    print_string (pretty_stmts  0 [stmt3;stmt2;stmt1] )
              

(*
(* AST of statements *)
(* The ints are all the line number of the (start of) the stmt *)
type stmt = 
  | Assign of id * exp * int
  | While of exp * stmt * int
  | Ite of exp * stmt * stmt * int
  | Stmts of stmt list * int
  | In of id * int
  | Out of id * int

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens *)
let rec parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list = 
  match toks with
  | [] -> raise (BadInput "End of file while parsing an expression")
  | (T.Ident i, ln) :: toks -> (Ident (i, ln), toks)
  | (T.Num n, _) :: toks -> (Num n, toks) 
  | (T.True, _) :: toks -> (Bool true, toks)
  | (T.False, _) :: toks -> (Bool false, toks)
  | (T.Lparen, ln) :: toks -> 
    (match parse_exp toks with
     | (e, (T.Op o, ln) :: toks2) ->
       (match parse_exp toks2 with
        | (e', (T.Rparen, ln) :: toks3) -> (Oper (e, (o, ln), e'), toks3)
        | _ -> raise (BadInput ("Missing ')' on line " ^ string_of_int ln)))
     | _ -> raise (BadInput ("Expected operator on line " ^ 
                             string_of_int ln)))
  | (_, ln) :: _ ->
    raise (BadInput ("Expected expression on line" ^ string_of_int ln))

(* Convert the first statement in toks into an AST. Return it with the left
   over tokens *)
let rec parse_stmt (toks : T.tok_loc list) : stmt * T.tok_loc list=
  match toks with
  | [] -> raise (BadInput "End of file while parsing a statement")
  | (T.Input, ln) :: (T.Ident x, _) :: toks -> (In (x, ln), toks)
  | (T.Output, ln) :: (T.Ident x, _) :: toks -> (Out (x, ln), toks)
  | (T.Ident x, ln) :: (T.Assign, _) :: toks -> 
    let (e, toks') = parse_exp toks in
    (Assign (x, e, ln), toks')
  | (T.While, ln) :: toks ->
    let (e, toks1) = parse_exp toks in
    let (s, toks2) = parse_stmt toks1 in
    (While (e, s, ln), toks2)
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (e, (T.Then, _) :: toks1) ->
       (match parse_stmt toks1 with
        | (s1, (T.Else, _) :: toks2) ->
          let (s2, toks3) = parse_stmt toks2 in
          (Ite (e, s1, s2, ln), toks3)
        | _ -> raise (BadInput "Expected else"))
     | _ -> raise (BadInput "Expected then"))
  | (T.Lcurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (Stmts (s_list, ln), toks)
  | (_,ln) :: _ -> 
    raise (BadInput ("Bad statement on line " ^ string_of_int ln))

(* Convert all of the statement in toks into an AST. Return them with the left
   over tokens *)
and parse_stmt_list (toks : T.tok_loc list) : stmt list * T.tok_loc list =
  match toks with
  | ((T.Rcurly, _) :: toks') -> ([], toks')
  | _ ->
    let (s, toks') = parse_stmt toks in
    let (s_list, toks'') = parse_stmt_list toks' in
    (s::s_list, toks'')

(* Repeatedly parse statments until the input is empty *)
(* NB, the difference between parse_stmt_list which can leave leftover tokens *) 
let rec parse_program (toks : T.tok_loc list) : stmt list = 
  match toks with
  | [] -> []
  | _ ->
    let (s, toks') = parse_stmt toks in
    let s_list = parse_program toks' in
    s::s_list

   *)

