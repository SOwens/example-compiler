open Util
module S = SourceAst

type var =
  | Vreg of int
  | Stack of int
  | NamedSource of string
  | NamedTmp of int
        [@@deriving show, ord]

module VarCmp = struct
  type t = var
  let compare = compare_var
end

module Varset' = Set.Make(VarCmp)

module Varset = struct
  include Varset'
  let show s = "Varset.of_list " ^ [%show: var list] (elements s)
  let pp f s = Format.fprintf f "%s" (show s)
end

module Varmap = Map.Make(VarCmp)

(* Atomic expressions, parameterised over what the identifiers are *)
type atomic_exp =
  | Ident of var
  | Num of Int64.t
  | Bool of bool
        [@@deriving show]

(* A single entry in a basic bloc *)
type block_elem = 
  | AssignOp of var * atomic_exp * Tokens.op * atomic_exp
  | AssignAtom of var * atomic_exp
  | Ld of var * atomic_exp
  | St of var * atomic_exp
  | In of var
  | Out of var
        [@@deriving show]

type basic_block = block_elem list
    [@@deriving show]

(* A basic block is either at the end of the program, or there is an
   unconditional jump out of it, or a branch out of it. This type represents
   the index of the next block in the cfg. *)
type next_block = 
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of var * int * int
                [@@deriving show]

(* An adjacency list representation for the CFG *)
type cfg_entry = { index : int; elems : block_elem list; next : next_block }
    [@@deriving show]
type cfg = cfg_entry list
    [@@deriving show]


(* Convert an expression to an atomic expression, along with a list of basic
   block entries that perform the computation of the expression. Essentially, 
   this flattens out the expression by using a temporary to store the results
   of each sub-expression *)
let exp_to_atomic (e : S.exp) : atomic_exp * basic_block =
  (* Generate unique names for temporary variables *)
  let next_ident = ref 1 in
  let get_ident () : var = 
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    NamedTmp x
  in
  let rec do_to_atom (e : S.exp) : atomic_exp * basic_block =
    match e with
    | S.Ident (i, _) -> (Ident (NamedSource i), [])
    | S.Num i -> (Num i, [])
    | S.Bool i -> (Bool i, [])
    | S.Oper (e1, (op, _), e2) ->
      let (a1, s1) = do_to_atom e1 in
      let (a2, s2) = do_to_atom e2 in
      let ident = get_ident () in
      (Ident ident, AssignOp (ident, a1, op, a2) :: s2 @ s1)
  in
  do_to_atom e

(* Like exp_to_atomic, but strip the Identifier off of the expression, 
   but fail if the atomic is a num or bool, which can only happen if the 
   source exp is a num or bool. *)
let exp_to_atomic_test (e : S.exp) : var * basic_block =
  match exp_to_atomic e with
  | (Ident i, stmts) -> (i, stmts)
  | ((Num _ | Bool _), _) -> raise (InternalError "exp_to_atomic")

(* Build the control-flow graph *)
let build_cfg (stmts : S.stmt list) : cfg =
  (* A counter to get new indices for blocks *)
  let next_block_num = ref 0 in
  let get_block_num () =
    let x = !next_block_num in
    next_block_num := 1 + !next_block_num;
    x
  in

  (* Store the cfg here as we build it *)
  let the_cfg = ref [] in
  let add_block (num : int) (block : basic_block) (next : next_block) : unit =
    the_cfg := { index = num; elems = List.rev block; next = next} :: !the_cfg
  in

  (* Convert stmts to basic blocks, and add them to the_cfg. block_num is the
     index for the first block, ret_block for the block to return to after
     stmts. block_acc accumulates the block that we've seen so far. *)
  let rec find_blocks (block_num : int) (ret_block : int) (stmts : S.stmt list) (block_acc : basic_block) : unit =
    match stmts with
    | [] -> 
      add_block block_num block_acc (Next ret_block)
    | S.In (x, _) :: s ->
      find_blocks block_num ret_block s (In (NamedSource x) :: block_acc)
    | S.Out (x, _) :: s -> 
      find_blocks block_num ret_block s (Out (NamedSource x) :: block_acc)
    | S.Assign (x, e, _) :: s1 ->
      let (a, s2) = exp_to_atomic e in
      find_blocks block_num ret_block s1 
        (AssignAtom (NamedSource x, a) :: s2 @ block_acc)
    | S.Stmts (s1, _) :: s2 ->
      find_blocks block_num ret_block (s1 @ s2) block_acc
    | S.While (e, s1, _) :: s2 ->
      let (i, s3) = exp_to_atomic_test e in
      let header_block_n = get_block_num () in
      let body_block_n = get_block_num () in
      add_block block_num block_acc (Next header_block_n);
      find_blocks body_block_n header_block_n [s1] [];
      if s2 = [] then
        add_block header_block_n s3 
          (Branch (i, body_block_n, ret_block))
      else
        let following_block_n = get_block_num () in
        add_block header_block_n s3 
          (Branch (i, body_block_n, following_block_n));
        find_blocks following_block_n ret_block s2 []
    | S.Ite (e, s1, s2, _) :: s3 ->
      let (i, s4) = exp_to_atomic_test e in
      let true_block_n = get_block_num () in
      let false_block_n = get_block_num () in
      add_block block_num (s4 @ block_acc) 
        (Branch (i, true_block_n, false_block_n));
      if s3 = [] then
        (find_blocks true_block_n ret_block [s1] [];
         find_blocks false_block_n ret_block [s2] [])
      else
        let following_block_n = get_block_num () in
        find_blocks true_block_n following_block_n [s1] [];
        find_blocks false_block_n following_block_n [s2] [];
        find_blocks following_block_n ret_block s3 []
  in

  let end_block_num = get_block_num () in
  let init_block = get_block_num () in
  add_block end_block_num [] End;
  find_blocks init_block end_block_num stmts [];
  !the_cfg
