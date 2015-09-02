open Util
open SourceAst

(* Statically evaluate an expression according to the identifier values in env *)
let rec fold_exp (env : exp Strmap.t) (exp : exp) : exp = 
  match exp with
  | Ident (i, ln) -> 
    (try Strmap.find i env 
     with Not_found -> exp)
  | Num n -> Num n
  | Bool b -> Bool b
  | Oper (e1, op, e2) ->
    let o1 = fold_exp env e1 in
    let o2 = fold_exp env e2 in
    (match (o1, op, o2) with
     | (Num n1, (T.Plus, _), Num n2) -> Num (Int64.add n1 n2)
     | (Num n1, (T.Minus, _), Num n2) -> Num (Int64.sub n1 n2)
     | (Num n1, (T.Times, _), Num n2) -> Num (Int64.mul n1 n2)
     | (Num n1, (T.Div, _), Num n2) when n2 <> 0L -> Num (Int64.div n1 n2)
     | (Num n1, (T.Lt, _), Num n2) -> Bool (n1 < n2)
     | (Bool b1, (T.And, _), Bool b2) -> Bool (b1 && b2)
     | (Bool b1, (T.Or, _), Bool b2) -> Bool (b1 || b2)
     | (Bool b1, (T.Eq, _), Bool b2) -> Bool (b1 = b2)
     | (Num n1, (T.Eq, _), Num n2) -> Bool (Int64.compare n1 n2 = 0)
     | _ -> Oper (o1, op, o2))

let is_const (e : exp) : bool =
  match e with
  | Num _ | Bool _ -> true
  | _ -> false

let same_const (e1 : exp) (e2 : exp) : bool =
  match (e1,e2) with
  | (Num n1, Num n2) -> Int64.compare n1 n2 = 0
  | (Bool b1, Bool b2) -> b1 = b2
  | _ -> false

(* If v1 and v2 contain the same constant, return it. Otherwise return None *) 
let merge_constants (id : id) (v1 : exp option) (v2 : exp option) : exp option = 
  match (v1,v2) with
  | (Some e1, Some e2) ->
    if same_const e1 e2 then
      Some e1
    else None
  | _ -> None

(* Do constant propagation. Accumulate an environment of definitely known
   constants at the end of stmts, given definitely known constants env at the
   start. *)
let rec prop_stmts (env : exp Strmap.t) (stmts : stmt list) : exp Strmap.t * stmt list = 
  match stmts with
  | [] -> (env,[])
  | In (x,ln) :: stmts -> 
    let (env',stmts') = prop_stmts (Strmap.remove x env) stmts in
    (env', In (x, ln) :: stmts')
  | Out (x,ln) :: stmts -> 
    let (env',stmts') = prop_stmts env stmts in
    (env', Out (x, ln) :: stmts')
  | Assign (x, e, ln) :: stmts -> 
    let o1 = fold_exp env e in
    let first_env = 
      if is_const o1 then Strmap.add x o1 env else Strmap.remove x env 
    in
    let (env',stmts') = prop_stmts first_env stmts in
      (env', Assign (x, o1, ln) :: stmts')
  | While (e, s1, ln) :: stmts ->
    let o1 = fold_exp env e in
    (match o1 with
     | Bool false -> prop_stmts env stmts
     | _ -> 
       let (env1,os1) = prop_loop_body env s1 in
       (* have to redo the condition, for the sound constants on entry *)
       let o1 = fold_exp env1 e in
       let (env',stmts') = prop_stmts env1 stmts in
       (env', While (o1, os1, ln) :: stmts'))
  | Ite (e, s1, s2, ln) :: stmts ->
    let o1 = fold_exp env e in
    (match o1 with
     | Bool true -> 
       let (env1, os1) = prop_stmt env s1 in
       let (env', stmts') = prop_stmts env1 stmts in
       (env', os1::stmts')
     | Bool false ->
       let (env2, os2) = prop_stmt env s2 in
       let (env', stmts') = prop_stmts env2 stmts in
       (env', os2::stmts')
     | _ -> 
       let (env1, os1) = prop_stmt env s1 in
       let (env2, os2) = prop_stmt env s2 in
       (* Only include constants that are known to be the same at the end of
          both branches of the Ite for the subsequent statements. *)
       let (env',stmts') = prop_stmts (Strmap.merge merge_constants env1 env2) stmts in
       (env', Ite (o1, os1, os2, ln) :: stmts'))
  | Stmts (stmts1, ln) :: stmts ->
    let (env1, os1) = prop_stmts env stmts1 in
    let (env', stmts') = prop_stmts env1 stmts in
    (env', Stmts (os1, ln) :: stmts')

and prop_stmt env stmt =
  match prop_stmts env [stmt] with
  | (env,[os]) -> (env,os)
  | _ -> raise (InternalError "prop_stmt")

(* Given possibly known constants env at the start, compute the definitely
   known constants at the end, assuming that stmt is run in a loop body an
   unknown number of times. *)
and prop_loop_body (env : exp Strmap.t) stmt : exp Strmap.t * stmt =
  let (env', stmt') = prop_stmt env stmt in
  (* The next approximation of constants at the start *)
  let env'' = Strmap.merge merge_constants env env' in
  if Strmap.equal same_const env env'' then
    (* Same as last time, fixed point reached *)
    (env',stmt')
  else
    (* Use the original statement body, because stmt' was computed with too
       many constants *)
    prop_loop_body env'' stmt
