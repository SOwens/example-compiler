open Util
open SourceAst
module T = Tokens

exception Todo

let rec unnest (stmts : stmt list) : stmt list =

  (* Generate unique names for temporary variables *)
  let next_ident = ref 1 in
  let get_ident () : id =
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    Temp x
  in

  let is_atomic (e : exp) : bool =
    match e with
    | Num _ -> true
    | Bool _ -> true
    | Ident (_, []) -> true
    | Ident (_, es) -> false
    | Op _ -> false
    | Uop _ -> false
    | Array _ -> false
  in

  let is_flat (e : exp) : bool =
    match e with
    | Num _ -> true
    | Bool _ -> true
    | Ident (_, []) -> true
    | Ident (_, es) -> List.for_all is_atomic es
    | Op (e1, op, e2) -> is_atomic e1 && is_atomic e2
    | Uop (uop, e) -> is_atomic e
    | Array es -> List.for_all is_atomic es
  in

  (* Turn off warning about unused is_flat *)
  let _ = ignore (is_flat (Num 1L)) in

  (* Flatten out and expression into a list of statements and an expression by
     using temporary variables to store the results of each sub-expression. The
     expression returned satisfies is_flat above, and the statement are
     assignments of flat expressions to non-array identifiers *)
  let rec unnest_exp (e : exp) : stmt list * exp =
    match e with
    | Num i -> ([], Num i)
    | Bool b -> ([], Bool b)
    | Ident (i, []) -> ([], Ident (i, []))
    | Ident (i, es) -> raise Todo
    | Op (e1, T.And, e2) -> raise Todo
    | Op (e1, T.Or, e2) -> raise Todo
    | Op (e1, op, e2) ->
      let (s1, f1) = unnest_exp e1 in
      let (s2, f2) = unnest_exp e2 in
      if is_atomic f1 && is_atomic f2 then
        (s1 @ s2, Op (f1, op, f2))
      else if is_atomic f1 then
        let id2 = get_ident () in
        (s1 @ s2 @ [Assign (id2, [], f2)],
         Op (f1, op, Ident (id2, [])))
      else if is_atomic f2 then
        let id1 = get_ident () in
        (s1 @ s2 @ [Assign (id1, [], f1)],
         Op (Ident (id1, []), op, f2))
      else
        let id1 = get_ident () in
        let id2 = get_ident () in
        (s1 @ s2 @ [Assign (id1, [], f1); Assign (id2, [], f2)],
         Op (Ident (id1, []), op, Ident (id2, [])))
    | Uop (uop, e) ->
      let (s, f) = unnest_exp e in
      if is_atomic f then
        (s, Uop (uop, f))
      else
        let id = get_ident () in
        (s @ [Assign (id, [], f)], Uop (uop, f))
    | Array es ->
      raise Todo
  in

  let stmts_to_stmt (s : stmt list) : stmt =
    match s with
    | [s1] -> s1
    | _ -> Stmts s
  in

  let rec unnest_stmt (s : stmt) : stmt list =
    match s with
    | Assign (x, [], e) ->
      let (s', f) = unnest_exp e in
        s' @ [Assign (x, [], f)]
    | Assign (x, es, e) ->
      raise Todo
    | DoWhile (s0, e, s1) ->
      let s0' = unnest_stmt s0 in
      let (se1, e') = unnest_exp e in
      let s1' = unnest_stmt s1 in
      [DoWhile (stmts_to_stmt (s0' @ se1), e', stmts_to_stmt s1')]
    | Ite (e, s1, s2) ->
      let (se, e') = unnest_exp e in
      let s1' = unnest_stmt s1 in
      let s2' = unnest_stmt s2 in
      se @ [Ite (e', stmts_to_stmt s1', stmts_to_stmt s2')]
    | Stmts s_list ->
      List.flatten (List.map unnest_stmt s_list)
    | In id -> [In id]
    | Out id -> [Out id]
    | Loc (stmt, int) ->
      unnest_stmt stmt
  in

  List.flatten (List.map unnest_stmt stmts)
