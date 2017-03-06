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

(* The type checker. In addition to checking the types, we build a new AST that
   annotates the scope of all of the variables. Each use of a variable either
   refers to a global variable, a local variable, or a function parameter. The
   type checker keeps track of which sort each variable is and annotates each
   use with this information. This allows the register allocator and back end
   to treat them differently. *)

open Util
open SourceAst
module T = Tokens

(* Types *)
type t =
  | Tint
  | Tbool
  | Tarray of int (* The number of dimensions that the array has *)

let show_t t =
  match t with
  | Tint -> "int"
  | Tbool -> "bool"
  | Tarray n -> "array " ^ string_of_int n

(* Map functions to their types, and variables to their types and scope *)
type env_t = { funs : (t list * t) Idmap.t; vars : (t * scope) Idmap.t }

(* Raise a type error *)
let type_error (ln : int option) (msg : string) : 'a =
  match ln with
  | Some ln ->
    raise (BadInput ("Type error on line " ^ string_of_int ln ^ ": " ^ msg))
  | None ->
    raise (BadInput ("Type error at unknown location: " ^ msg))

(* Annotate an identifier with its scope *)
let add_scope (id : id) (s : scope) : id =
  match id with
  | Source (i, None) -> Source (i, Some s)
  | Source (_, Some _) ->
    raise (InternalError "scoped identifier during type checking")
  | Temp (x, y) -> Temp (x, y)

(* Compute the type of an expression, or raise BadInput if there is a type
   error. Also return a new scope-annotated version of the expression. *)
let rec type_exp (ln : int option) (env : env_t) (e : exp) : t * exp =
  match e with
  | Ident (i, es) ->
    let (t, scope) =
      try Idmap.find i env.vars
      with Not_found ->
        type_error ln ("reference to undeclared variable " ^ show_id i)
    in
    let (ts, es') = List.split (List.map (type_exp ln env) es) in
    let l = List.length ts in
    let annotated_exp = Ident (add_scope i scope, es') in
    (* If there are no indices, then just use the type of the identifier, even
       if it is of array type. If there are indices, they must fully index the
       array, which always contains ints *)
    if l = 0 then
      (t, annotated_exp)
    else
      (match t with
       | Tarray num_dims ->
         if num_dims = l && List.for_all (fun x -> x = Tint) ts then
           (Tint, annotated_exp)
         else if num_dims = l then
           type_error ln "array index with non-integer type"
         else
           type_error ln ("array reference with " ^ string_of_int l ^
                          " indices, expected " ^ string_of_int num_dims)
       | _ ->
         type_error ln ("attempt to index non-array variable " ^ show_id i))
  | Call (f, args) ->
    let (param_types, ret_type) =
      try Idmap.find f env.funs
      with Not_found ->
        type_error ln ("call to undefined function " ^ show_id f)
    in
    let (ts, es') = List.split (List.map (type_exp ln env) args) in
    (* A local function to check that the parameter and argument types match
       up. Use n to keep track of which position, for the error message. Assume
       that the lists are of the same length. *)
    let rec check (n : int) (pts : t list) (ats : t list) : unit =
      match (pts, ats) with
      | ([], []) -> ()
      | (pt::pts, at::ats) ->
        if pt = at then check (n+1) pts ats else
          type_error ln ("function call given argument of type " ^ show_t at ^
                         " but expected type " ^ show_t pt ^
                         " in argument number " ^ string_of_int n)
      | _ -> assert false
    in
    if List.length param_types <> List.length args then
      type_error ln ("function call expects " ^
                     string_of_int (List.length param_types) ^
                     " arguments, given " ^
                     string_of_int (List.length ts))
    else
      check 1 param_types ts;
    (ret_type, Call (f, es'))
  | Num n -> (Tint, Num n)
  | Bool b -> (Tbool, Bool b)
  | Op (e1, op, e2) ->
    let (t1, e1') = type_exp ln env e1 in
    let (t2, e2') = type_exp ln env e2 in
    let t =
      match (t1, op, t2) with
      | (Tbool, (T.And | T.Or | T.Eq), Tbool) -> Tbool
      | (Tint, (T.Plus | T.Minus | T.Times | T.Div | T.Lshift | T.BitOr |
                T.BitAnd), Tint) ->
        Tint
      | (Tint, (T.Lt | T.Eq | T.Gt), Tint) -> Tbool
      | (t1, _, t2) ->
        type_error ln ("operator " ^ T.show_op op ^
                       " applied to expressions of types " ^ show_t t1 ^
                       " and " ^ show_t t2)
    in
    (t, Op (e1', op, e2'))
  | Uop (uop, e) ->
    let (t, e') = type_exp ln env e in
    (match (uop, t) with
     | (T.Not, Tbool) -> (Tbool, Uop (uop, e'))
     | (T.Not, t) ->
       type_error ln ("operator " ^ T.show_uop uop ^
                      " applied to expression of type " ^ show_t t))
  | Array es ->
    let (ts, es') = List.split (List.map (type_exp ln env) es) in
    let l = List.length ts in
    if l = 0 then
      type_error ln "attempt to allocate array with 0 dimensions"
    else if List.for_all (fun x -> x = Tint) ts then
      (Tarray l, Array es')
    else
      type_error ln "array dimension with non-integer type"

(* Type check an identifier without array indices *)
let type_simple_ident (ln : int option) (env : env_t) (i : id) : t * id =
  match type_exp ln env (Ident (i, [])) with
  | (t, Ident (i', [])) -> (t, i')
  | _ -> assert false (* type_exp does not change the shape of the expression *)

(* Type check a statement. Raise BadInput if there is an error. return gives
   the return type of the enclosing function. ln gives the current line number.
   Return a scope annotated version of the statement. Also strip the location
   annotations.
*)
let rec type_stmt (ln : int option) (env :env_t) (return : t) (stmt : stmt)
  : stmt =
  match stmt with
  | In i ->
    let (t, i') = type_simple_ident ln env i in
    if t <> Tint then
      type_error ln "input with non-integer type"
    else
      In i'
  | Out i ->
    let (t, i') = type_simple_ident ln env i in
    if t <> Tint then
      type_error ln "output with non-integer type"
    else
      Out i'
  | Return (Some i) ->
    let (t, i') = type_simple_ident ln env i in
    if t <> return then
      type_error ln ("return has type " ^ show_t t ^
                     " in a function with return type " ^
                     show_t return)
    else
      Return (Some i')
  | Return None ->
    type_error ln "return without value"
  | Assign (x, es, e) ->
    (match type_exp ln env (Ident (x, es)) with
     | (t1, Ident (x', es')) ->
       let (t2, e') = type_exp ln env e in
       if t1 <> t2 then
         type_error ln ("assignment to variable of type " ^ show_t t1 ^ " with type " ^
                        show_t t2)
       else
         Assign (x', es', e')
     | _ -> assert false)
  | DoWhile (s1, e, s2) ->
    let s1' = type_stmt ln env return s1 in
    (match type_exp ln env e with
     | (Tbool, e') ->
       let s2' = type_stmt ln env return s2 in
       DoWhile (s1', e', s2')
     | _ ->
      type_error ln "do/while test of non-bool type")
  | Ite (e, s1, s2) ->
    (match type_exp ln env e with
     | (Tbool, e') ->
       let s1' = type_stmt ln env return s1 in
       let s2' = type_stmt ln env return s2 in
       Ite (e', s1', s2')
     | _ -> type_error ln "if test of non-bool type")
  | Stmts (s_list) ->
    Stmts (List.map (type_stmt ln env return) s_list)
  | Loc (s, ln') ->
    type_stmt (Some ln') env return s

let source_typ_to_t (t : SourceAst.typ) : t =
  match t with
  | Int -> Tint
  | Bool -> Tbool
  | Array n -> Tarray n

(* Get the declared types of all of the parameters. Raise an exception if a
   duplicate name is found, using the location ln. Accumulate the answer in
   param_env. *)
let rec get_param_types (ln : int option) (params : (id * typ) list)
    (param_env : (t * scope) Idmap.t)
  : (t * scope) Idmap.t =
  match params with
  | [] -> param_env
  | (x,t)::params ->
    if Idmap.mem x param_env then
      type_error ln ("duplicate function parameter " ^ show_id x)
    else
      get_param_types ln params
        (Idmap.add x (source_typ_to_t t, Parameter) param_env)

(* Get the declared types of all of the variables.  Raise an exception if a
   duplicate is found. Accumulate the answer in var_env. *)
let rec get_var_types (s : scope) (vars : var_dec list)
    (var_env : (t * scope) Idmap.t)
  : (t * scope) Idmap.t =
  match vars with
  | [] -> var_env
  | v::vars ->
    if Idmap.mem v.var_name var_env then
      type_error v.loc ("duplicate variable definition " ^ show_id v.var_name)
    else
      get_var_types s vars
        (Idmap.add v.var_name (source_typ_to_t v.typ, s) var_env)

(* Check the init expressions on a variable declaration. Return a
   scope-annotated version of the declaration. *)
let type_var_dec (s : scope) (env : env_t) (dec : var_dec) : var_dec =
  let (t, init') = type_exp dec.loc env dec.init in
  if t = source_typ_to_t dec.typ then
    { dec with var_name = add_scope dec.var_name s; init = init' }
  else
    type_error dec.loc ("initialisation of variable typed " ^
                        show_t (source_typ_to_t dec.typ) ^
                        " to expression of type " ^ show_t t)

(* See the documentation for Map.merge in the OCaml standard library *)
let merge_keep_first _ (x : 'a option) (y : 'a option) : 'a option =
  match (x,y) with
  | (None, None) -> None
  | (Some x, Some _) -> Some x
  | (Some x, None) -> Some x
  | (None, Some y) -> Some y

(* Determine whether that each control-flow path through the stmts must include
   a return.  This way we know that a function cannot fall off the end, without
   returning a value of the correct type. *)
let rec check_return_paths (stmts : stmt list) : bool =
  match stmts with
  | [] -> false
  | Return _ :: _ -> true
  | s::stmts ->
    if check_return_paths stmts then
      true
    else (* The remainder might not return, so the first statement must *)
      match s with
      | Return _ -> assert false (* already checked for this case *)
      | In _ | Out _ | Assign _ -> false
      | Loc (s,_) -> check_return_paths [s]
      | Ite (_,s1,s2) ->
        check_return_paths [s1] && check_return_paths [s2]
      | DoWhile (s1, _, _) ->
        check_return_paths [s1]
      | Stmts s -> check_return_paths s

(* Check a function. Return a scope-annotated version is there are no errors. *)
let type_function (env : env_t) (f : func) : func =
  let param_env = get_param_types f.loc f.params Idmap.empty in
  let local_env = get_var_types Local f.locals Idmap.empty in
  (* Add the parameter and locals to the env. Ensure that the locals shadow the
     parameters and globals, and that the parameters shadow the globals. *)
  let env1 = Idmap.merge merge_keep_first param_env env.vars in
  let env2 = Idmap.merge merge_keep_first local_env env1 in
  let new_env = { env with vars = env2 } in
  let locals' = List.map (type_var_dec Local new_env) f.locals in
  let body' =
    List.map (type_stmt None new_env (source_typ_to_t f.ret)) f.body
  in
  if not (check_return_paths f.body) then
    type_error f.loc "function might not return"
  else
    { f with
      params = List.map (fun (i,t) -> (add_scope i Parameter, t)) f.params;
      locals = locals';
      body = body' }

(* Get the declared types of all of the functions. Raise an exception if a
   duplicate is found. Accumulate the answer in fun_env. *)
let rec get_function_types (funcs : func list) (fun_env : (t list * t) Idmap.t)
  : (t list * t) Idmap.t =
  match funcs with
  | [] -> fun_env
  | f::funcs ->
    if Idmap.mem f.fun_name fun_env then
      type_error f.loc ("duplicate function definition " ^ show_id f.fun_name)
    else
      get_function_types funcs
        (Idmap.add f.fun_name
           (List.map (fun (_,t) -> source_typ_to_t t) f.params,
            source_typ_to_t f.ret)
           fun_env)

let type_prog (p : prog) : prog =
  (* Get the types of the globals and functions. *)
  let env =
    { funs = get_function_types p.funcs Idmap.empty;
      vars = get_var_types Global p.globals Idmap.empty }
  in
  (* Check the function bodies and global initialisations in the environment
     with types for all of the globals and functions. This means that the
     top-level is one recursive scope and each function body and global
     initialisation can refer to any of the others (or itself). *)
  let globals' = List.map (type_var_dec Global env) p.globals in
  let funcs' = List.map (type_function env) p.funcs in
  { funcs = funcs'; globals = globals' }
