module T = Tokens 
type id = string [@@deriving show]

type exp = 
  (* the int is the line number of the identifier *)
  | Ident of id * int
  | Num of Int64.t
  | Bool of bool
  (* the int is the line number of the operator *)
  | Oper of exp * (T.op * int) * exp
              [@@deriving show]

(* the ints are all the line number of the (start of) the stmt *)
type stmt = 
  | Assign of id * exp * int
  | While of exp * stmt * int
  | Ite of exp * stmt * stmt * int
  | Stmts of stmt list * int
  | In of id * int
  | Out of id * int
             [@@deriving show]

val parse_program : (Tokens.token * int) list -> stmt list
