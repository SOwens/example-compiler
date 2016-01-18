open BlockStructure
type linear = 
  | Instr of block_elem
  | CJump of var * bool * string (* jump to string if var is bool *)
  | Jump of string
  | Label of string
  [@@deriving show]

type linear_list = linear list
  [@@deriving show]

val cfg_to_linear : cfg -> linear list
