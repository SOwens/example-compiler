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
  (* The int is the line number of the identifier *)
  | Ident of id * exp list * int
  | Num of Int64.t
  | Bool of bool
  (* The int is the line number of the operator *)
  | Oper of exp * (T.op * int) * exp
  (* Allocate a new array of given dimensions. Initialise to 0 *)
  | Array of exp list * int
  [@@deriving show]

(* AST of statements *)
(* The ints are all the line number of the (start of) the stmt *)
type stmt =
  | Assign of id * exp list * exp * int
  | While of exp * stmt * int
  | Ite of exp * stmt * stmt * int
  | Stmts of stmt list * int
  | In of id * int
  | Out of id * int
  [@@deriving show]

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens *)
let rec parse_atomic_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing an expression")
  | (T.Ident i, ln) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Ident (i, indices, ln), toks)
  | (T.Num n, _) :: toks -> (Num n, toks)
  | (T.True, _) :: toks -> (Bool true, toks)
  | (T.False, _) :: toks -> (Bool false, toks)
  | (T.Lparen, ln) :: toks ->
    (match parse_atomic_exp toks with
     | (e, (T.Op o, ln) :: toks2) ->
       (match parse_atomic_exp toks2 with
        | (e', (T.Rparen, ln) :: toks3) -> (Oper (e, (o, ln), e'), toks3)
        | _ -> raise (BadInput ("Missing ')' on line " ^ string_of_int ln)))
     | _ -> raise (BadInput ("Expected operator on line " ^
                             string_of_int ln)))
  | (T.Array, l) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Array (indices, l), toks)
  | (_, ln) :: _ ->
    raise (BadInput ("Expected expression on line" ^ string_of_int ln))

and parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  let (exp1, toks) = parse_atomic_exp toks in
  match toks with
  | (T.Op o, ln) :: toks ->
    let (exp2, toks) = parse_atomic_exp toks in
    (Oper (exp1, (o, ln), exp2), toks)
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
     | _ -> raise (BadInput ("Missing ']' on line " ^ string_of_int l)))
  | _ ->
    ([], toks)

(* Convert the first statement in toks into an AST. Return it with the left
   over tokens *)
let rec parse_stmt (toks : T.tok_loc list) : stmt * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing a statement")
  | (T.Input, ln) :: (T.Ident x, _) :: toks -> (In (x, ln), toks)
  | (T.Output, ln) :: (T.Ident x, _) :: toks -> (Out (x, ln), toks)
  | (T.Ident x, ln) :: (T.Assign, _) :: toks ->
    let (e, toks) = parse_exp toks in
    let (indices, toks) = parse_indices toks in
    (Assign (x, indices, e, ln), toks)
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
