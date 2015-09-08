type 'reg atomic_exp =
  | Ident of 'reg
  | Num of Int64.t
  | Bool of bool
        [@@deriving show]

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

type 'reg next_block = 
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of 'reg * int * int
                [@@deriving show]

type 'reg cfg_entry = { index : int; elems : 'reg block_elem list; next : 'reg next_block }
    [@@deriving show]
type 'reg cfg = 'reg cfg_entry list
    [@@deriving show]

val build_cfg : SourceAst.stmt list -> SourceAst.id cfg
