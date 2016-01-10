type var =
  | Vreg of int
  | Stack of int
  | NamedSource of string
  | NamedTmp of int
        [@@deriving show]

module Varset : sig
  include Set.S with type elt = var
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Varmap : sig
  include Map.S with type key = var
end

type atomic_exp =
  | Ident of var
  | Num of Int64.t
  | Bool of bool
        [@@deriving show]

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

type next_block = 
  | End
  | Next of int
  (* The first int is the block number if the ident is true, and the second if
   * it is false *)
  | Branch of var * int * int
                [@@deriving show]

type cfg_entry = { index : int; elems : block_elem list; next : next_block }
    [@@deriving show]
type cfg = cfg_entry list
    [@@deriving show]

val build_cfg : SourceAst.stmt list -> cfg
