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

(* The language's AST, and a predictive, recursive descent parser. See the
   ../README.md for the grammar. *)

open Util
module T = Tokens

type id =
  | Source of string
  | Temp of string * int
  [@@deriving show, ord]

let show_id i =
  match i with
  | Source s -> s
  | Temp (s,i) -> "_tmp_" ^ s ^ string_of_int i

module Idord = struct
  type t = id
  let compare = compare_id
end

module Idmap = Map.Make(Idord)

(* AST of expressions *)
type exp =
  | Ident of id * exp list
  | Num of int64
  | Bool of bool
  | Op of exp * T.op * exp
  | Uop of T.uop * exp
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

let pp_stmts fmt (stmts : stmt list) : unit =
  Format.fprintf fmt "%a"
    (pp_list pp_stmt) stmts

(* Raise a parse error *)
let parse_error (ln : int) (msg : string) : 'a =
  raise (BadInput ("Parse error on line " ^ string_of_int ln ^ ": " ^ msg))

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens. *)
let rec parse_atomic_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing an expression")
  | (T.Ident i, ln) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Ident (Source i, indices), toks)
  | (T.Num n, _) :: toks -> (Num n, toks)
  | (T.True, _) :: toks -> (Bool true, toks)
  | (T.False, _) :: toks -> (Bool false, toks)
  | (T.Op T.Minus, _) :: toks ->
    let (e, toks) = parse_atomic_exp toks in
    (Op (Num 0L, T.Minus, e), toks)
  | (T.Uop uop, _) :: toks ->
    let (e, toks) = parse_atomic_exp toks in
    (Uop (uop, e), toks)
  | (T.Array, _) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Array indices, toks)
  | (T.Lparen, ln) :: toks ->
    (match parse_exp toks with
     | (e, (T.Rparen, _) :: toks) ->
       (e, toks)
     | _ -> parse_error ln "'(' without matching ')'")
  | (_, ln) :: _ ->
    parse_error ln "Bad expression"

and parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match parse_atomic_exp toks with
  | (e1, (T.Op o, ln) :: toks) ->
    let (e2, toks) = parse_atomic_exp toks in
    (Op (e1, o, e2), toks)
  | (e1, toks) -> (e1, toks)

(* Parse 0 or more array indices. Return them with the left over tokens. *)
and parse_indices (toks : T.tok_loc list) : exp list * T.tok_loc list =
  match toks with
  | (T.Lbrac, l) :: toks ->
    (match parse_exp toks with
     | (e, (T.Rbrac, _) :: toks) ->
       let (es,toks) = parse_indices toks in
       (e::es, toks)
     | _ -> parse_error l "'[' without matching ']'")
  | _ -> ([], toks)

(* Convert the first statement in toks into an AST. Return it with the left
   over tokens *)
let rec parse_stmt (toks : T.tok_loc list) : stmt * T.tok_loc list =
  match toks with
  | [] -> raise (BadInput "End of file while parsing a statement")
  | (T.Ident x, ln) :: toks ->
    (match parse_indices toks with
     | (indices, (T.Assign, _) :: toks) ->
       let (e, toks) = parse_exp toks in
       (Loc (Assign (Source x, indices, e), ln), toks)
     |_ -> parse_error ln "expected ':=' after identifier")
  | (T.While, ln) :: toks ->
    let (e, toks) = parse_exp toks in
    let (s, toks) = parse_stmt toks in
    (Loc (DoWhile (Stmts [], e, s), ln), toks)
  | (T.Do, ln) :: toks ->
    (match parse_stmt toks with
     | (s, (T.While, _)::toks) ->
       let (e, toks) = parse_exp toks in
       (Loc (DoWhile (s, e, Stmts []), ln), toks)
     | _ -> parse_error ln "'do' without 'while'")
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (e, (T.Then, _) :: toks) ->
       (match parse_stmt toks with
        | (s1, (T.Else, _) :: toks) ->
          let (s2, toks) = parse_stmt toks in
          (Loc (Ite (e, s1, s2), ln), toks)
        | _ -> parse_error ln "'if' without 'else'")
     | _ ->  parse_error ln "'if' without 'then")
  | (T.Lcurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (Loc (Stmts (s_list), ln), toks)
  | (T.Input, ln) :: (T.Ident x, _) :: toks -> (Loc (In (Source x), ln), toks)
  | (T.Output, ln) :: (T.Ident x, _) :: toks -> (Loc (Out (Source x), ln), toks)
  | (_,ln) :: _ ->
    parse_error ln "Bad statement"

(* Convert all of the statement in toks into an AST, stopping on a }. Return
   them with the left over tokens *)
and parse_stmt_list (toks : T.tok_loc list) : stmt list * T.tok_loc list =
  match toks with
  | ((T.Rcurly, _) :: toks) -> ([], toks)
  | _ ->
    let (s, toks) = parse_stmt toks in
    let (s_list, toks) = parse_stmt_list toks in
    (s::s_list, toks)

(* Repeatedly parse statments until the input is empty *)
(* NB, the difference between parse_stmt_list which can leave leftover tokens *)
let rec parse_program (toks : T.tok_loc list) : stmt list =
  match toks with
  | [] -> []
  | _ ->
    let (s, toks) = parse_stmt toks in
    let s_list = parse_program toks in
    s::s_list

let stmts_to_stmt (s : stmt list) : stmt =
  match s with
  | [s1] -> s1
  | _ -> Stmts s
