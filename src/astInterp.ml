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

(* An interpreter for ASTs. This is meta circular in the sense that we use
   OCaml refs for mutable variables, and we represent arrays with mutable OCaml
   arrays. We also model function returns with exceptions. *)

open SourceAst
module T = Tokens

(* For when the interpreter crashed, such as array bounds violations *)
exception Crash of string

(* For errors that a well-typed program can't have *)
exception TypeError

(* Values are either integers or n-dimensional arrays of integers.
   We keep multi-dimensional arrays in a single dimensional one and include a
   list of how big each dimension is.
   We represent bools as numbers: true = 1L and false = 0L *)
type val_t =
  | Vint of int64
  | Varray of int list * int64 array

let val_t_to_int (v : val_t) : int64 =
  match v with
  | Vint i -> i
  | Varray _ -> raise TypeError

let val_t_to_array (v : val_t) : int list * int64 array =
  match v with
  | Vint 0L -> raise (Crash "null pointer exception")
  | Vint _ -> raise TypeError
  | Varray (dims, a) -> (dims, a)

(* Given the array's dimensions, work out the slot for a particular set of
   indices. Sizes and indices must have the same length.  Return None if one
   of the indices is out of bounds, i.e. greater than the size. *)
let array_address (sizes : int list) (indices : int list) : int option =
  (* acc keeps track of the product of the dimensions seen so far *)
  let rec f sizes indices (acc : int) =
    match (sizes, indices) with
    | ([], []) -> 0
    | (s1::sizes, i1::indices) ->
        i1 * acc + f sizes indices (s1 * acc)
    | _ ->
      raise TypeError
  in
  if List.for_all2 (fun s i -> i < s) sizes indices then
    Some (f sizes indices 1)
  else
    None

let bool_to_int64 (b : bool) : int64 =
  if b then 1L else 0L

let int64_to_bool (i : int64) : bool =
  if Int64.compare i 0L = 0 then false else true

(* Do a primitive operation, but not && or || *)
let do_op op (n1 : int64) (n2 : int64) : int64 =
  match op with
  | T.Plus -> Int64.add n1 n2
  | T.Minus -> Int64.sub n1 n2
  | T.Times -> Int64.mul n1 n2
  | T.Div ->
    if n2 = 0L then
      raise (Crash "Division by 0")
    else
      Int64.div n1 n2
  | T.Lt -> bool_to_int64 (Int64.compare n1 n2 < 0)
  | T.Gt -> bool_to_int64 (Int64.compare n1 n2 > 0)
  | T.Eq -> bool_to_int64 (Int64.compare n1 n2 = 0)
  | T.Lshift ->
    let i = Int64.to_int n2 in
    (* Int64.shift_left is wacky outside of this interval *)
    if 0 <= i && i < 64 then
      Int64.shift_left n1 i
    else
      0L
  | T.BitOr -> Int64.logor n1 n2
  | T.BitAnd -> Int64.logand n1 n2
  | T.And | T.Or -> assert false

let do_uop uop (n : int64) : int64 =
  match uop with
  | T.Not ->
    bool_to_int64 (not (int64_to_bool n))

(* The environment will have the code of all of the functions that we can call,
   it will also keep the variable bindings in a map of OCaml references.
   Assignments to a variables will be interpreted as assignments to the
   corresponding reference. Note that an assignment to a variable (of scalar or
   array type) mutates the binding for that variable in the environment,
   whereas an assignment to an array element (i.e., an array update), actually
   changes the array value without modifying the environment binding for the
   array.
*)
type env_t = { funs : func Idmap.t; vars : val_t ref Idmap.t }

(* To model return control flow *)
exception Return_exn of val_t

(* Add the function's arguments to the environment env at a call site. Assume
   that the two input lists are the same length. This is ensured by the type
   checker. We also know there are no duplicate parameter names. *)
let rec add_arguments (params : (id * _) list) (args : val_t list) (env : env_t)
  : env_t =
  match (params, args) with
  | ([], []) -> env
  | ((x,_)::params, v::args) ->
    (* Allocate a mutable reference to store the parameter's value in. Then the
       function's body can assign to the variable by changing the binding. *)
    let binding = ref v in
    add_arguments params args { env with vars = Idmap.add x binding env.vars }
  | (_, _) ->
    raise TypeError

(* Compute the value of an expression *)
let rec interp_exp (env : env_t) (e : exp) : val_t =
  match e with
  | Ident (i, []) ->
    !(Idmap.find i env.vars) (* i will be in the environment in a well-typed
                                program *)
  | Ident (i, iexps) ->
    let (sizes, a) = val_t_to_array !(Idmap.find i env.vars) in
    let indices =
      List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp env e))))
        iexps
    in
    (match array_address sizes indices with
     | Some x -> Vint (Array.get a x)
     | None -> raise (Crash "array index out of bounds"))
  | Call (f, args) ->
    (* f will be in the environment in a well-typed program *)
    let func = Idmap.find f env.funs in
    let arg_vals = List.map (interp_exp env) args in
    let new_env1 = add_arguments func.params arg_vals env in
    let new_env2 = interp_var_decs new_env1 func.locals in
    (try
       List.iter (interp_stmt new_env2) func.body;
       (* We know this cannot happen because of the type checker, each path in
          each function must have a return. Otherwise, we would have to return
          a default value *of the right type* *)
       raise TypeError
     with Return_exn v -> v)
  | Num n -> Vint n
  | Bool b -> Vint (bool_to_int64 b)
  | Op (e1, T.And, e2) ->
    (match interp_exp env e1 with
     | Vint n ->
       if int64_to_bool n then
         interp_exp env e2
       else
         Vint n
     | _ -> raise TypeError)
   | Op (e1, T.Or, e2) ->
    (match interp_exp env e1 with
     | Vint n ->
       if int64_to_bool n then
         Vint n
       else
         interp_exp env e2
     | _ -> raise TypeError)
  | Op (e1, op, e2) ->
    (match (interp_exp env e1, interp_exp env e2) with
     | (Vint n1, Vint n2) -> Vint (do_op op n1 n2)
     | _ -> raise TypeError)
  | Uop (uop, e) ->
    (match interp_exp env e with
     | Vint n -> Vint (do_uop uop n)
     | _ -> raise TypeError)
  | Array iexps ->
    let indices =
      List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp env e))))
        iexps
    in
    Varray (indices,
            Array.make (List.fold_right (fun x y -> x * y) indices 1) 0L)

(* Run the initialisation expressions of the variables, and add bindings to the
   resulting values into the environment *)
and interp_var_decs (env : env_t) (decs : var_dec list) : env_t =
  (* Start the variables at 0, because they can be referenced before they are
     initialised. NB, this allows null pointer exceptions (or crashes) into the
     language, since the variable can have array type.  This is a consequence
     of having each global and local scope be a single recursive scope. *)
  let new_env =
    List.fold_right
      (fun v env ->
        let binding = ref (Vint 0L) in
         Idmap.add v.var_name binding env)
      decs
      env.vars
  in
  let env = { env with vars = new_env } in
  (* Now run all of the initialisation expressions *)
  List.iter
    (fun d ->
       let v = interp_exp env d.init in
       Idmap.find d.var_name env.vars := v)
    decs;
  env

(* Run a statement *)
and interp_stmt (env : env_t) (s : stmt) : unit =
  match s with
  | Assign (i, [], e) ->
    let v = interp_exp env e in
    Idmap.find i env.vars := v
  | Assign (i, iexps, e) ->
    (match !(Idmap.find i env.vars) with
     | Varray (sizes, a) ->
       (let indices =
          List.map (fun e -> (Int64.to_int (val_t_to_int (interp_exp env e))))
            iexps
        in
        match array_address sizes indices with
        | None -> raise (Crash "array index out of bounds")
        | Some x ->
          let v = val_t_to_int (interp_exp env e) in
          Array.set a x v)
     | Vint 0L -> raise (Crash "null pointer exception")
     | Vint _ -> raise TypeError)
  | DoWhile (head_s, e, body_s) ->
    interp_stmt env head_s;
    if int64_to_bool (val_t_to_int (interp_exp env e)) then
      (interp_stmt env body_s;
       interp_stmt env s)
    else
      ()
  | Ite (e, s1, s2) ->
    if int64_to_bool (val_t_to_int (interp_exp env e)) then
      interp_stmt env s1
    else
      interp_stmt env s2
  | Stmts sl ->
    List.iter (interp_stmt env) sl
  | In i ->
    Printf.printf "> ";
    (try
       let n = Int64.of_string (read_line ()) in
       Idmap.find i env.vars := Vint n
     with Failure _ -> raise (Crash "not a 64-bit integer"))
  | Out i ->
    (print_string (Int64.to_string (val_t_to_int !(Idmap.find i env.vars)));
     print_newline ())
  | Return None ->
    raise (Return_exn (Vint 0L))
  | Return (Some i) ->
    raise (Return_exn !(Idmap.find i env.vars))
  | Loc (s, _) -> interp_stmt env s

let interp_prog (p : prog) : unit =
  let fun_env =
    List.fold_right (fun f env -> Idmap.add f.fun_name f env)
      p.funcs Idmap.empty
  in
  ignore (interp_var_decs { funs = fun_env; vars = Idmap.empty } p.globals)
