open Util
open BlockStructure

(* The basic type signature of a monad *)
module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Add an operation for getting the number of a variable, and for running the
   computation *)
module type VarNumMonad = sig
  include Monad
  val get_var_num : SourceAst.id -> int t
  val run : 'a t -> 'a
end

(* Implement the monad *)
module M : VarNumMonad = struct
  (* A monadic computation will be a function from the current map and next
     number to use to a new map, new next number, and result value *)
  type 'a t = int Strmap.t * int -> int Strmap.t * int * 'a

  (* return is defined as a standard state monad *)
  let return x (map, next) = (map, next, x)

  (* bind is defined as a standard state monad *)
  let bind x f (map, next) = 
    let (new_map, new_next, res) = x (map, next) in
    f res (new_map, new_next)

  (* if the variable is in the map, use the existing index, otherwise extend the map *)
  let get_var_num v (map, next) =
    try (map, next, Strmap.find v map) with
    | Not_found ->
      let new_map = Strmap.add v next map in
      let new_next = next + 1 in
      (new_map, new_next, next)

  let run f =
    let (_, _, x) = f (Strmap.empty, 0) in 
    x
end 

let sequence (l : 'a M.t list) : 'a list M.t = 
  let mcons p q = 
    M.do_ ;
    x <-- p;
    y <-- q;
    return (x::y) in
  List.fold_right mcons l (M.return [])

let mapM (f : 'a -> 'b M.t) (al : 'a list) : 'b list M.t = 
  sequence (List.map f al)

let alloc_local_vars_ae (ae : SourceAst.id atomic_exp) : (int atomic_exp) M.t =
  match ae with
  | Ident r -> 
    M.do_ ;
    i <-- M.get_var_num r;
    return (Ident i)
  | Num x -> M.return (Num x)
  | Bool b -> M.return (Bool b)

let alloc_local_vars_be (be : SourceAst.id block_elem) : (int block_elem) M.t =
  match be with
  | AssignOp (r, ae1, op, ae2) ->
    M.do_ ;
    i <-- M.get_var_num r;
    ae1' <-- alloc_local_vars_ae ae1;
    ae2' <-- alloc_local_vars_ae ae2;
    return (AssignOp (i, ae1', op, ae2'))
  | AssignAtom (r, ae) ->
    M.do_ ;
    i <-- M.get_var_num r;
    ae' <-- alloc_local_vars_ae ae;
    return (AssignAtom (i, ae'))
  | Ld (r, ae) ->
    M.do_ ;
    i <-- M.get_var_num r;
    ae' <-- alloc_local_vars_ae ae;
    return (Ld (i, ae'))
  | St (r, ae) ->
    M.do_ ;
    i <-- M.get_var_num r;
    ae' <-- alloc_local_vars_ae ae;
    return (St (i, ae'))
  | In r ->
    M.do_ ;
    i <-- M.get_var_num r;
    return (In i)
  | Out r ->
    M.do_ ;
    i <-- M.get_var_num r;
    return (Out i)

let alloc_local_vars_nb (nb : SourceAst.id next_block) : (int next_block) M.t =
  match nb with
  | End -> M.return End
  | Next i -> M.return (Next i)
  | Branch (r, t1, t2) ->
    M.do_ ;
    i <-- M.get_var_num r;
    return (Branch (i, t1, t2))

let alloc_local_vars_cfg (cfg : SourceAst.id cfg) : int cfg =
  let m =
    mapM 
      (fun e ->
         M.do_ ;
         new_elems <-- mapM alloc_local_vars_be e.elems;
         new_next <-- alloc_local_vars_nb e.next;
         return { index = e.index; elems = new_elems; next = new_next })
      cfg
  in
  M.run m
