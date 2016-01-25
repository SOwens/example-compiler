open Util
open SourceAst

exception Todo

(*
let rec unnest (stmts : stmt list) : stmt list =

  (* Generate unique names for temporary variables *)
  let next_ident = ref 1 in
  let get_ident () : var =
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    NamedTmp x
  in

  let is_atomic (e : exp) : bool =
    match e with
    | Num i -> true
    | Bool _ -> raise (InternalError "Bool in blockStructure")
    | Ident (i, []) -> true
    | Ident (i, es) -> raise Todo
    | Oper (e1, op, e2) -> false
    | Array es -> false
  in

  (* Flatten out and expression into a list of statements and an expression by
     using temporary variables to store the results of each sub-expression. *)
  let rec unnest_exp (e : exp) : stmt list * exp =
    match e with
    | Num i -> ([], Num i)
    | Bool _ -> raise (InternalError "Bool in blockStructure")
    | Ident (i, []) -> ([], Ident (i, []))
    | Ident (i, es) -> raise Todo
    | Oper (e1, op, e2) ->
      if is_atomic e1 && is_atomic e2 then
        ([], e)
      else
        let (s1, a1) = unnest_exp e1 in
        let (s2, a2) = unnest_exp e2 in
        let ident = get_ident () in
        (Assign (ident, [], Oper (a1, op, a2)) :: s2 @ s1, Ident (ident, []))
    | Array es ->
      raise Todo
  in

  let rec unnest_stmt (s : stmt) : stmt list =
    match s with
    | Assign (x, [], e) ->
      let (s', e') = unnest_exp e in
        s' @ [Assign (x, [], e')]
    | Assign (x, es, e) ->
      raise Todo
    | While (e, s) ->
      let (s1, e1) = unnest_exp e in
      let s2 = unnest_stmt s in
      While (e1, s2)



    |
    | S.Ident (i, es) -> raise Todo
   | S.Array [] ->
      raise (InternalError "0-dim array in blockStructure")
    | S.Array es ->
      raise Todo
  in

  (* Like exp_to_atomic, but strip the Identifier off of the expression,
     but fail if the atomic is a num or bool, which can only happen if the
     source exp is a num or bool. *)
  let exp_to_atomic_test (e : S.exp) : var * basic_block =
    match exp_to_atomic e with
    | (Ident i, stmts) -> (i, stmts)
    | (Num _, _) -> raise (InternalError "exp_to_atomic")
  in

*)
