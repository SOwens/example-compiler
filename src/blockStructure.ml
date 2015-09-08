open Util
module S = SourceAst

(* Atomic expressions, parameterised over what the identifiers are *)
type 'reg atomic_exp =
  | Ident of 'reg
  | Num of Int64.t
  | Bool of bool
        [@@deriving show]

(* A single entry in a basic bloc *)
type 'reg block_elem = 
  | AssignOp of 'reg * 'reg atomic_exp * Tokens.op * 'reg atomic_exp
  | AssignAtom of 'reg * 'reg atomic_exp
  | Ld of 'reg * 'reg atomic_exp
  | St of 'reg * 'reg atomic_exp
  | In of 'reg
  | Out of 'reg
        [@@deriving show]

type 'reg basic_block = 'reg block_elem list
    [@@deriving show]

(* A basic block is either at the end of the program, or there is an
   unconditional jump out of it, or a branch out of it. This type represents
   the index of the next block in the cfg. *)
type 'reg next_block = 
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of 'reg * int * int
                [@@deriving show]

(* An adjacency list representation for the CFG *)
type 'reg cfg_entry = { index : int; elems : 'reg block_elem list; next : 'reg next_block }
    [@@deriving show]
type 'reg cfg = 'reg cfg_entry list
    [@@deriving show]


(* Convert an expression to an atomic expression, along with a list of basic
   block entries that perform the computation of the expression. Essentially, 
   this flattens out the expression by using a temporary to store the results
   of each sub-expression *)
let exp_to_atomic (e : S.exp) : S.id atomic_exp * S.id basic_block =
  (* Generate unique names for temporary variables *)
  let next_ident = ref 0 in
  let get_ident () : string = 
    let x = !next_ident in
    next_ident := (!next_ident) + 1;
    "_tmp" ^ string_of_int x
  in
  let rec do_to_atom (e : S.exp) : S.id atomic_exp * S.id basic_block =
    match e with
    | S.Ident (i, _) -> (Ident i, [])
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
let exp_to_atomic_test (e : S.exp) : S.id * S.id basic_block =
  match exp_to_atomic e with
  | (Ident i, stmts) -> (i, stmts)
  | ((Num _ | Bool _), _) -> raise (InternalError "exp_to_atomic")

(* Build the control-flow graph *)
let build_cfg (stmts : S.stmt list) : S.id cfg =
  (* A counter to get new indices for blocks *)
  let next_block_num = ref 0 in
  let get_block_num () =
    let x = !next_block_num in
    next_block_num := 1 + !next_block_num;
    x
  in

  (* Store the cfg here as we build it *)
  let the_cfg = ref [] in
  let add_block (num : int) (block : S.id basic_block) (next : S.id next_block) : unit =
    the_cfg := { index = num; elems = List.rev block; next = next} :: !the_cfg
  in

  (* Convert stmts to basic blocks, and add them to the_cfg. block_num is the
     index for the first block, ret_block for the block to return to after
     stmts. block_acc accumulates the block that we've seen so far. *)
  let rec find_blocks (block_num : int) (ret_block : int) (stmts : S.stmt list) (block_acc : S.id basic_block) : unit =
    match stmts with
    | [] -> 
      add_block block_num block_acc (Next ret_block)
    | S.In (x, _) :: s ->
      find_blocks block_num ret_block s (In x :: block_acc)
    | S.Out (x, _) :: s -> 
      find_blocks block_num ret_block s (Out x :: block_acc)
    | S.Assign (x, e, _) :: s1 ->
      let (a, s2) = exp_to_atomic e in
      find_blocks block_num ret_block s1 
        (AssignAtom (x, a) :: s2 @ block_acc)
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
