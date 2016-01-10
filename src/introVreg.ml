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
  val lookup_var : var -> var t
  val run : 'a t -> 'a
end

(* Implement the monad *)
module M : VarNumMonad = struct
  (* A monadic computation will be a function from the current map and next
     number to use to a new map, new next number, and result value *)
  type 'a t = var Varmap.t * int -> var Varmap.t * int * 'a

  (* return is defined as a standard state monad *)
  let return x (map, next) = (map, next, x)

  (* bind is defined as a standard state monad *)
  let bind x f (map, next) = 
    let (new_map, new_next, res) = x (map, next) in
    f res (new_map, new_next)

  (* if the variable is in the map, use the existing index, otherwise extend the map *)
  let lookup_var v (map, next) =
    try (map, next, Varmap.find v map) with
    | Not_found ->
      let new_map = Varmap.add v (Vreg next) map in
      let new_next = next + 1 in
      (new_map, new_next, Vreg next)

  let run f =
    let (_, _, x) = f (Varmap.empty, 0) in 
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

let intro_vreg_ae (ae : atomic_exp) : atomic_exp M.t =
  match ae with
  | Ident r -> 
    M.do_ ;
    i <-- M.lookup_var r;
    return (Ident i)
  | Num x -> M.return (Num x)
  | Bool b -> M.return (Bool b)

let intro_vreg_be (be : block_elem) : block_elem M.t =
  match be with
  | AssignOp (r, ae1, op, ae2) ->
    M.do_ ;
    i <-- M.lookup_var r;
    ae1' <-- intro_vreg_ae ae1;
    ae2' <-- intro_vreg_ae ae2;
    return (AssignOp (i, ae1', op, ae2'))
  | AssignAtom (r, ae) ->
    M.do_ ;
    i <-- M.lookup_var r;
    ae' <-- intro_vreg_ae ae;
    return (AssignAtom (i, ae'))
  | Ld (r, ae) ->
    M.do_ ;
    i <-- M.lookup_var r;
    ae' <-- intro_vreg_ae ae;
    return (Ld (i, ae'))
  | St (r, ae) ->
    M.do_ ;
    i <-- M.lookup_var r;
    ae' <-- intro_vreg_ae ae;
    return (St (i, ae'))
  | In r ->
    M.do_ ;
    i <-- M.lookup_var r;
    return (In i)
  | Out r ->
    M.do_ ;
    i <-- M.lookup_var r;
    return (Out i)

let intro_vreg_nb (nb : next_block) : next_block M.t =
  match nb with
  | End -> M.return End
  | Next i -> M.return (Next i)
  | Branch (r, t1, t2) ->
    M.do_ ;
    i <-- M.lookup_var r;
    return (Branch (i, t1, t2))

let intro_vreg (cfg : cfg) : cfg =
  let m =
    mapM 
      (fun e ->
         M.do_ ;
         new_elems <-- mapM intro_vreg_be e.elems;
         new_next <-- intro_vreg_nb e.next;
         return { index = e.index; elems = new_elems; next = new_next })
      cfg
  in
  M.run m
