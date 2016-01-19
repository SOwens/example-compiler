type op = 
  | Plus
  | Minus
  | Times
  | Div
  | Lt
  | Gt
  | Eq
  | And
  | Or
  | Lshift
  | BitOr
  | BitAnd
      [@@deriving show]

val op_to_string : op -> string

type token = 
  | Num of Int64.t
  | Ident of string
  | Op of op
  | Lparen
  | Rparen
  | Lcurly
  | Rcurly
  | While
  | If
  | Then
  | Else
  | Assign
  | True
  | False
  | Input
  | Output
      [@@deriving show]

type tok_loc = (token * int)
    [@@deriving show]

val lex : string -> int -> int -> tok_loc list
