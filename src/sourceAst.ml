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

(* The language's AST, and a predictive, recursive descent parser. See the
   ../README.md for the grammar. *)

open Util
module T = Tokens
module F = Format

(* Type of identifiers. Source ones come from the original program, and Temp
   ones come from intermediate compilation stages. This makes it easy to avoid
   unintentional conflicts. The string on a Temp should name the stage that
   introduced it. The scope indicates where the identifier is bound, if that is
   known. We expect the parse to generate unknown scopes and for the type
   checker to fill them in. All Temps are considered local in scope. *)
type scope =
  | Global
  | Parameter
  | Local

type id =
  | Source of string * scope option
  | Temp of string * int

let show_scope (s : scope) : string =
  match s with
  | Global -> "Global"
  | Parameter -> "Parameter"
  | Local -> "Local"

let show_id (i : id) : string =
  match i with
  | Source (s, None) -> s
  | Source (s, Some scope) -> s ^ "_" ^ show_scope scope
  | Temp (s,i) -> "_tmp_" ^ s ^ string_of_int i

(* Pretty-print an identifier *)
let pp_id (fmt : Format.formatter) (i : id) : unit =
  F.fprintf fmt "%s" (show_id i)

(* Construct a total order on ids so that we can use them as keys in maps.
   OCaml maps are implemented with balanced binary trees *)
let scope_to_int s =
  match s with
  | Global -> 0
  | Parameter -> 1
  | Local -> 2

(* Compare two scopes *)
let compare_scope (s1 : scope) (s2 : scope) : int =
  compare (scope_to_int s1) (scope_to_int s2)

(* Compare two identidiers *)
let compare_id (id1 : id) (id2 : id) : int =
  match (id1, id2) with
  | (Source (s1, scope1), Source (s2, scope2)) ->
    let c =
      option_compare compare_scope scope1 scope2
    in
    if c = 0 then
      String.compare s1 s2
    else
      c
  | (Temp _, Source _) -> -1
  | (Source _, Temp _) -> 1
  | (Temp (s1,i1), Temp (s2,i2)) ->
    let c = String.compare s1 s2 in
    if c = 0 then
      compare i1 i2
    else
      c

module Idord = struct
  type t = id
  let compare = compare_id
end

(* Build Map module specialised to keys of type id *)
module Idmap = Map.Make(Idord)

(* AST of expressions *)
type exp =
  | Ident of id * exp list
  | Call of id * exp list
  | Num of int64
  | Bool of bool
  | Op of exp * T.op * exp
  | Uop of T.uop * exp
  (* Allocate a new array of given dimensions. Initialise to 0 *)
  | Array of exp list

(* pretty printing for expressions *)

(* Print a list, where each element is enclosed in [] *)
let rec pp_array_list (f : F.formatter -> 'a -> unit)
    (fmt : F.formatter) (l : 'a list)
  : unit =
  match l with
  | [] -> ()
  | (h::t) ->
    F.fprintf fmt "[@[%a@]]@,%a"
      f h
      (pp_array_list f) t

(* Print a list, surrounded by (), separating the items with , *)
let pp_call (f : F.formatter -> 'a -> unit) (fmt : F.formatter)
    (l : 'a list)
  : unit =
  let rec pp fmt l =
    match l with
    | [] -> ()
    | [h] ->
      F.fprintf fmt "%a"
        f h
    | (h::t) ->
      F.fprintf fmt "%a,@ %a"
        f h
        pp t
  in
  F.fprintf fmt "(@[%a@])"
    pp l

(* Pretty print an expression *)
let rec pp_exp (fmt : F.formatter) (exp : exp) : unit =
  match exp with
  | Ident (id, []) ->
    F.fprintf fmt "%a"
      pp_id id
  | Ident (id, es) ->
    F.fprintf fmt "%a%a"
      pp_id id
      (pp_array_list pp_exp) es
  | Call (id, es) ->
    F.fprintf fmt "%a%a"
      pp_id id
      (pp_call pp_exp) es
  | Num n -> F.fprintf fmt "%Ld" n
  | Bool true -> F.fprintf fmt "true"
  | Bool false -> F.fprintf fmt "false"
  | Op (e1, op, e2) ->
    F.fprintf fmt "(@[%a@ %s@ %a@])"
      pp_exp e1
      (Tokens.show_op op)
      pp_exp e2
  | Uop (uop, e) ->
    F.fprintf fmt "@[<2>%s@ %a@]"
      (Tokens.show_uop uop)
      pp_exp e
  | Array es ->
    F.fprintf fmt "@[<2>array@ %a@]"
      (pp_array_list pp_exp) es

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
  | Return of id option
  | Loc of stmt * int (* annotate a statement with it's source line number *)

(* Pretty-print a statement *)
let rec pp_stmt (fmt : F.formatter) (stmt : stmt) : unit =
  match stmt with
  | Assign (id, [], e) ->
    F.fprintf fmt "@[<2>%a :=@ %a@]"
      pp_id id
      pp_exp e
  | Assign (id, es, e) ->
    F.fprintf fmt "@[<2>%a%a :=@ %a@]"
      pp_id id
      (pp_array_list pp_exp) es
      pp_exp e
  | DoWhile (Stmts [], e, s) ->
    F.fprintf fmt "@[<2>while@ %a@ %a@]"
      pp_exp e
      pp_stmt s
  | DoWhile (s, e, Stmts []) ->
    F.fprintf fmt "@[<2>do@ %a@ while@ %a@]"
      pp_stmt s
      pp_exp e
  | DoWhile (s1, e, s2) ->
    F.fprintf fmt "@[<2>do@ %a@ while@ %a@ %a@]"
      pp_stmt s1
      pp_exp e
      pp_stmt s2
  | Ite (e, s1, s2) ->
    F.fprintf fmt "@[<2>if@ %a@ then@ %a@ else@ %a@]"
      pp_exp e
      pp_stmt s1
      pp_stmt s2
  | Stmts slist ->
    F.fprintf fmt "{@\n%a}"
      pp_stmts slist
  | In i ->
    F.fprintf fmt "@[<2>input@ %a@]"
      pp_id i
  | Out i ->
    F.fprintf fmt "@[<2>output@ %a@]"
      pp_id i
  | Return None ->
    F.fprintf fmt "return"
  | Return (Some i) ->
    F.fprintf fmt "@[<2>return@ %a@]"
      pp_id i
  | Loc (s, _) ->
    pp_stmt fmt s

and pp_stmts (fmt : F.formatter) (stmts : stmt list) : unit =
  let rec pp fmt stmts =
    match stmts with
    | [] -> ()
    | stmt::stmts ->
      F.fprintf fmt "%a@\n%a"
        pp_stmt stmt
        pp stmts
  in
  F.fprintf fmt "@[<v>%a@]"
    pp stmts

(* AST of types *)
type typ =
  | Int
  | Bool
  (* An int array with the given number of dimensions *)
  | Array of int

(* Pretty-print a type *)
let pp_typ (fmt : F.formatter) (t : typ) : unit =
  match t with
  | Int -> F.fprintf fmt "int"
  | Bool -> F.fprintf fmt "bool"
  | Array n -> F.fprintf fmt "array@ %d" n

(* AST of variable and function declarations *)
type var_dec = { var_name : id; typ : typ; init : exp; loc : int option }

type func = { fun_name : id; params : (id * typ) list; ret : typ;
              locals : var_dec list; body : stmt list; loc : int option}

(* Pretty-print a variable declaration *)
let pp_var_dec (fmt : F.formatter) (d : var_dec) : unit =
  F.fprintf fmt "@[<2>let@ %a@ :@ %a@ =@ %a@]"
    pp_id d.var_name
    pp_typ d.typ
    pp_exp d.init

(* Pretty-print variable declarations, 1 per line *)
let pp_var_decs (fmt : F.formatter) (decs : var_dec list) : unit =
  let rec pp fmt decs =
    match decs with
    | [] -> ()
    | var_dec::decs ->
      F.fprintf fmt "%a@\n%a"
        pp_var_dec var_dec
        pp decs
  in
  F.fprintf fmt "@[<v>%a@]"
    pp decs

(* Pretty-print function parameters *)
let rec pp_params (fmt : F.formatter) (params : (id * typ) list) : unit =
  match params with
  | [] -> ()
  | (n,t)::params ->
    F.fprintf fmt "@[(%a@ :@ %a)@]@ %a"
      pp_id n
      pp_typ t
      pp_params params

(* Pretty-print a function *)
let pp_func (fmt : F.formatter) (func : func) : unit =
  F.fprintf fmt "@[<2>function@ %a@ %a@ :@ %a@ {@\n%a%a}@]@\n"
    pp_id func.fun_name
    pp_params func.params
    pp_typ func.ret
    pp_var_decs func.locals
    pp_stmts func.body

(* AST of complete programs *)
type prog = { globals : var_dec list; funcs : func list }

(* Pretty-print a program *)
let pp_program (fmt : F.formatter) (p : prog) : unit =
  pp_var_decs fmt p.globals;
  List.iter (pp_func fmt) p.funcs

(* Raise a parse error *)
let parse_error (ln : int) (msg : string) : 'a =
  raise (BadInput ("Parse error on line " ^ string_of_int ln ^ ": " ^ msg))

(* Raise a parse error explaining the expected token and the token actually there *)
let parse_error_expect (ln : int) (given : T.token) (expect : T.token)
    (where : string)
  : 'a =
  raise (BadInput ("Parse error on line " ^ string_of_int ln ^ ": expected " ^
                   T.show_token expect ^ " in " ^ where ^ " but found " ^
                   T.show_token given))


(* Raise a parse error because the end of file was reached unexpectedly *)
let eof_error (expect : string) : 'a =
  raise (BadInput ("Parse error: end of file while parsing " ^ expect))

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens. *)
let rec parse_atomic_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match toks with
  | [] -> eof_error "an expression"
  | (T.Ident i, ln) :: (T.Lparen, _) ::toks ->
    let (args, toks) = parse_args ln toks in
    (Call (Source (i,None), args), toks)
  | (T.Ident i, _) :: toks ->
    let (indices, toks) = parse_indices toks in
    (Ident (Source (i,None), indices), toks)
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
  | (T.Lparen, _) :: toks ->
    (match parse_exp toks with
     | (_, []) -> eof_error "a parenthesized expression"
     | (e, (T.Rparen, _) :: toks) ->
       (e, toks)
     | (_, (t, ln)::_) ->
       parse_error_expect ln t T.Rparen "a parenthesized expression")
  | (t, ln) :: _ ->
    parse_error ln ("bad expression, beginning with " ^ T.show_token t)

and parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match parse_atomic_exp toks with
  | (e1, (T.Op o, _) :: toks) ->
    let (e2, toks) = parse_atomic_exp toks in
    (Op (e1, o, e2), toks)
  | (e1, toks) -> (e1, toks)

(* Parse 0 or more array indices. Return them with the left over tokens. *)
and parse_indices (toks : T.tok_loc list) : exp list * T.tok_loc list =
  match toks with
  | (T.Lbrac, _) :: toks ->
    (match parse_exp toks with
     | (_, []) -> eof_error "an array index"
     | (e, (T.Rbrac, _) :: toks) ->
       let (es,toks) = parse_indices toks in
       (e::es, toks)
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.Rbrac "an array index")
  | _ -> ([], toks)

(* Parse 1 or more comma-separated expressions. Return them with the left over
   tokens. End on a closing parenthesis *)
and parse_args ln (toks : T.tok_loc list) : exp list * T.tok_loc list =
  match parse_exp toks with
  | (_, []) -> eof_error "a function call expression"
  | (e, (T.Comma, _)::toks) ->
    let (es, toks) = parse_args ln toks in
    (e::es, toks)
  | (e, (T.Rparen, _)::toks) ->
    ([e], toks)
  | (_, (t, ln) :: _) ->
    parse_error ln ("expected " ^ T.show_token T.Comma ^ " or " ^
                    T.show_token T.Rparen ^
                    " in a function call expression but found " ^
                    T.show_token t)

(* Convert the first statement in toks into an AST. Return it with the left
   over tokens *)
let rec parse_stmt (toks : T.tok_loc list) : stmt * T.tok_loc list =
  match toks with
  | [] -> eof_error "a statement"
  | (T.Ident x, ln) :: toks ->
    (match parse_indices toks with
     | (_, []) -> eof_error "an assignment statement"
     | (indices, (T.Assign, _) :: toks) ->
       let (e, toks) = parse_exp toks in
       (Loc (Assign (Source (x, None), indices, e), ln), toks)
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.Assign "an assignment statement")
  | (T.While, ln) :: toks ->
    let (e, toks) = parse_exp toks in
    let (s, toks) = parse_stmt toks in
    (Loc (DoWhile (Stmts [], e, s), ln), toks)
  | (T.Do, ln) :: toks ->
    (match parse_stmt toks with
     | (_, []) -> eof_error "a do statement"
     | (s, (T.While, _)::toks) ->
       let (e, toks) = parse_exp toks in
       (Loc (DoWhile (s, e, Stmts []), ln), toks)
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.While "a do statement")
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (_, []) -> eof_error "an if statement"
     | (e, (T.Then, _) :: toks) ->
       (match parse_stmt toks with
        | (_, []) -> eof_error "an if statement"
        | (s1, (T.Else, _) :: toks) ->
          let (s2, toks) = parse_stmt toks in
          (Loc (Ite (e, s1, s2), ln), toks)
        | (_, (t, ln) :: _) ->
          parse_error_expect ln t T.Else "an if statement")
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.Then "an if statement")
  | (T.Lcurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (Loc (Stmts (s_list), ln), toks)
  | (T.Input, ln) :: (T.Ident x, _) :: toks -> (Loc (In (Source (x,None)), ln), toks)
  | (T.Output, ln) :: (T.Ident x, _) :: toks -> (Loc (Out (Source (x,None)), ln), toks)
  | (T.Return, ln) :: (T.Ident x, _) :: toks -> (Loc (Return (Some (Source (x,None))), ln), toks)
  | (t, ln) :: _ ->
    parse_error ln ("bad statement, beginning with a " ^ T.show_token t)

(* Convert all of the statement in toks into an AST, stopping on a }. Return
   them with the left over tokens, not including the } *)
and parse_stmt_list (toks : T.tok_loc list) : stmt list * T.tok_loc list =
  match toks with
  | ((T.Rcurly, _) :: toks) -> ([], toks)
  | _ ->
    let (s, toks) = parse_stmt toks in
    let (s_list, toks) = parse_stmt_list toks in
    (s::s_list, toks)

(* Convert the first typ in toks into an AST. Return it with the left over
   tokens. *)
let parse_typ (toks : T.tok_loc list) : typ * T.tok_loc list =
  match toks with
  | [] -> eof_error "a type"
  | (T.Int, _) :: toks -> (Int, toks)
  | (T.Bool, _) :: toks -> (Bool, toks)
  | (T.Array, _) :: (T.Num n, _) :: toks -> (Array (Int64.to_int n), toks)
  | (t,ln) :: _ ->
    parse_error ln ("bad type, beginning with a " ^ T.show_token t)

(* Convert the first function parameter toks into an AST. Return it with the
   left over tokens. *)
let parse_param (toks : T.tok_loc list) : (id * typ) * T.tok_loc list =
  match toks with
  | [] -> eof_error "a function parameter"
  | (T.Lparen, _) :: (T.Ident x, _) :: (T.Colon,_) :: toks ->
    (match parse_typ toks with
     | (_, []) -> eof_error "a function parameter"
     | (t, (T.Rparen, _)::toks) ->
       ((Source (x,None), t), toks)
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.Rparen "a function parameter")
  | (t, ln) :: _ -> parse_error_expect ln t T.Lparen "a function parameter"

(* Convert a list of function parameters in toks into an AST. Return it with
   the left over tokens. Recognise when the list is over by the next token not
   being Lparen *)
let rec parse_param_list (toks : T.tok_loc list) : (id * typ) list * T.tok_loc list =
  match toks with
  | ((T.Lparen, _) :: _) ->
    let (v, toks) = parse_param toks in
    let (v_list, toks) = parse_param_list toks in
    (v::v_list, toks)
  | _ -> ([], toks)


(* Convert the first variable declaration in toks into an AST. Return it with
   the left over tokens. *)
let parse_var_dec (toks : T.tok_loc list) : var_dec * T.tok_loc list =
  match toks with
  | [] -> eof_error "a variable declaration"
  | (T.Let, ln) :: (T.Ident x, _) :: (T.Colon,_) :: toks ->
    (match parse_typ toks with
     | (_, []) -> eof_error "a variable declaration"
     | (t, (T.Op T.Eq, _)::toks) ->
       let (e, toks) = parse_exp toks in
       ({ var_name = Source (x,None); typ = t; init = e; loc = Some ln }, toks)
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t (T.Op T.Eq) "a variable declaration")
  | (t, ln) :: _ -> parse_error_expect ln t T.Let "a variable declaration"

(* Convert a list of variable declaration in toks into an AST. Return it with
   the left over tokens. Recognise when the list is over by the next token not
   being Let *)
let rec parse_var_dec_list (toks : T.tok_loc list) : var_dec list * T.tok_loc list =
  match toks with
  | (T.Let, _) :: _ ->
    let (v, toks) = parse_var_dec toks in
    let (v_list, toks) = parse_var_dec_list toks in
    (v::v_list, toks)
  | _ -> ([], toks)

(* Convert the first function declaration in toks into an AST. Return it with
   the left over tokens. *)
let parse_func (toks : T.tok_loc list) : func * T.tok_loc list =
  match toks with
  | [] -> eof_error "a function declaration"
  | (T.Function, ln) :: (T.Ident x, _) :: toks ->
    (match parse_param_list toks with
     | (_, []) -> eof_error "a function declaration"
     | (params, (T.Colon, _) :: toks) ->
       if List.length params <> 0 then
         (match parse_typ toks with
          | (_, []) -> eof_error "a function declaration"
          | (t, (T.Lcurly, _) :: toks) ->
            let (var_decs, toks) = parse_var_dec_list toks in
            let (stmts, toks) = parse_stmt_list toks in
            ({ fun_name = Source (x,None);
               params = params;
               ret = t;
               locals = var_decs;
               body = stmts;
               loc = Some ln }, toks)
          | (_, (t, ln) :: _) ->
            parse_error_expect ln t T.Lcurly "a function declaration")
       else
         parse_error ln
           "bad function declaration, functions must have at least 1 parameter"
     | (_, (t, ln) :: _) ->
       parse_error_expect ln t T.Colon "a function declaration")
  | (t, ln) :: _ -> parse_error_expect ln t T.Function "a function declaration"

(* Parse global variable declarations, and then function declarations until the
   end of file *)
let parse_program (toks : T.tok_loc list) : prog =
  let (var_decs, toks) = parse_var_dec_list toks in
  let rec parse_funs toks =
    match toks with
    | [] -> []
    | _ ->
      let (f, toks) = parse_func toks in
      f :: parse_funs toks
  in
  { globals = var_decs; funcs = parse_funs toks }

let stmts_to_stmt (s : stmt list) : stmt =
  match s with
  | [s1] -> s1
  | _ -> Stmts s
