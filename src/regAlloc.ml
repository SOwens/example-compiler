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
  val inc_var : var -> unit t
  val get_counts : int Varmap.t t
  val run : 'a t -> 'a
end

(* Implement the monad *)
module M : VarNumMonad = struct
  (* A monadic computation will be a function from the current map and next
     number to use to a new map, new next number, and result value *)
  type 'a t = int Varmap.t -> int Varmap.t * 'a

  (* return is defined as a standard state monad *)
  let return x map = (map, x)

  (* bind is defined as a standard state monad *)
  let bind x f map = 
    let (new_map, res) = x map in
    f res new_map

  let inc_var v map =
    try 
      let i = Varmap.find v map in
      (Varmap.add v (i + 1) map, ())
    with
    | Not_found ->
      (Varmap.add v 1 map, ())

  let get_counts map = (map, map)

  let run f =
    let (_, x) = f Varmap.empty in 
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

let sequence_ (l : unit M.t list) : unit M.t = 
  let mcons p q = 
    M.do_ ;
    x <-- p;
    y <-- q;
    return () in
  List.fold_right mcons l (M.return ())

let mapM_ (f : 'a -> unit M.t) (al : 'a list) : unit M.t = 
  sequence_ (List.map f al)


let count_vars_ae (ae : atomic_exp) : unit M.t =
  match ae with
  | Ident r -> 
    M.do_ ;
    () <-- M.inc_var r;
    return ()
  | Num x -> M.return ()
  | Bool b -> M.return ()

let count_vars_be (be : block_elem) : unit M.t =
  match be with
  | AssignOp (r, ae1, op, ae2) ->
    M.do_ ;
    () <-- M.inc_var r;
    () <-- count_vars_ae ae1;
    () <-- count_vars_ae ae2;
    return ()
  | AssignAtom (r, ae) ->
    M.do_ ;
    () <-- M.inc_var r;
    () <-- count_vars_ae ae;
    return ()
  | Ld (r, ae) ->
    M.do_ ;
    () <-- M.inc_var r;
    () <-- count_vars_ae ae;
    return ()
  | St (r, ae) ->
    M.do_ ;
    () <-- M.inc_var r;
    () <-- count_vars_ae ae;
    return ()
  | In r ->
    M.do_ ;
    () <-- M.inc_var r;
    return ()
  | Out r ->
    M.do_ ;
    () <-- M.inc_var r;
    return ()

let count_vars_nb (nb : next_block) : unit M.t =
  match nb with
  | End -> M.return ()
  | Next i -> M.return ()
  | Branch (r, t1, t2) ->
    M.do_ ;
    () <-- M.inc_var r;
    return ()

let count_vars (cfg : cfg) : int Varmap.t =
  let m =
    M.do_ ;
    () <-- mapM_
      (fun e ->
         M.do_ ;
         new_elems <-- mapM count_vars_be e.elems;
         new_next <-- count_vars_nb e.next;
         return ())
      cfg;
    map <-- M.get_counts;
    return map
  in
  M.run m
