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

(* The language's AST, and a recursive descent parser *)

open Util
module T = Tokens

type id = string [@@deriving show]

(* AST of expressions *)
type exp =
  | Ident of id * exp list
  | Num of int64
  | Bool of bool
  | Oper of exp * T.op * exp
  (* Allocate a new array of given dimensions. Initialise to 0 *)
  | Array of exp list
  [@@deriving show]

(* AST of statements *)
type stmt =
  | Assign of id * exp list * exp
  (* A generalised do/while loop. Always execute the first statement, then
     the test, then repeatedly do the 2nd, then first statement and then test 
     'while e s' becomes DoWhile (Stmts [], e, s) and 'do s while e' becomes
     DoWhile (s, e, Stmts []) *)
  | DoWhile of stmt * exp * stmt
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | In of id
  | Out of id
  | Loc of stmt * int (* annotate a statement with it's source line number *)
  [@@deriving show]

(* Raise a parse error *)
let parse_error (ln : int) (msg : string) : 'a =
  raise (BadInput ("Parse error on line " ^ string_of_int ln ^ ": " ^ msg))

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens *)
let rec parse_atomic_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing an expression")
  | (T.Ident i, ln) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Ident (i, indices), toks)
  | (T.Num n, _) :: toks -> (Num n, toks)
  | (T.True, _) :: toks -> (Bool true, toks)
  | (T.False, _) :: toks -> (Bool false, toks)
  | (T.Lparen, ln) :: toks ->
    (match parse_atomic_exp toks with
     | (e, (T.Op o, ln') :: toks2) ->
       (match parse_atomic_exp toks2 with
        | (e', (T.Rparen, ln'') :: toks3) -> (Oper (e, o, e'), toks3)
        | _ -> parse_error ln "'(' without matching ')'")
     | _ -> parse_error ln "'(' without following operator")
  | (T.Array, l) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Array indices, toks)
  | (_, ln) :: _ ->
    parse_error ln "Bad expression"

and parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  let (exp1, toks) = parse_atomic_exp toks in
  match toks with
  | (T.Op o, ln) :: toks ->
    let (exp2, toks) = parse_atomic_exp toks in
    (Oper (exp1, o, exp2), toks)
  | _ ->
    (exp1, toks)

(* Parse 0 or more array indices *)
and parse_indices (toks : T.tok_loc list) : exp list * T.tok_loc list =
  match toks with
  | (T.Lbrac, l) :: toks ->
    let (exp, toks) = parse_exp toks in
    (match toks with
     | (T.Rbrac, _) :: toks ->
       let (exps,toks) = parse_indices toks in
       (exp::exps, toks)
     | _ -> parse_error l "'[' without matching ']'")
  | _ ->
    ([], toks)

(* Convert the first statement in toks into an AST. Return it with the left
   over tokens *)
let rec parse_stmt (toks : T.tok_loc list) : stmt * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing a statement")
  | (T.Input, ln) :: (T.Ident x, _) :: toks -> (Loc (In x, ln), toks)
  | (T.Output, ln) :: (T.Ident x, _) :: toks -> (Loc (Out x, ln), toks)
  | (T.Ident x, ln) :: (T.Assign, _) :: toks ->
    let (e, toks) = parse_exp toks in
    let (indices, toks) = parse_indices toks in
    (Loc (Assign (x, indices, e), ln), toks)
  | (T.While, ln) :: toks ->
    let (e, toks1) = parse_exp toks in
    let (s, toks2) = parse_stmt toks1 in
    (Loc (DoWhile (Stmts [], e, s), ln), toks2)
  | (T.Do, ln) :: toks ->
    let (s, toks1) = parse_stmt toks in
    (match toks1 with
     | (T.While, _)::toks2 ->
       let (e, toks3) = parse_exp toks2 in
       (Loc (DoWhile (s, e, Stmts []), ln), toks3)
     | _ -> parse_error ln "'do' without 'while'")
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (e, (T.Then, _) :: toks1) ->
       (match parse_stmt toks1 with
        | (s1, (T.Else, _) :: toks2) ->
          let (s2, toks3) = parse_stmt toks2 in
          (Loc (Ite (e, s1, s2), ln), toks3)
        | _ -> parse_error ln "'if' without 'else'")
     | _ ->  parse_error ln "'if' without 'then")
  | (T.Lcurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (Loc (Stmts (s_list), ln), toks)
  | (_,ln) :: _ ->
    parse_error ln "Bad statement"

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
