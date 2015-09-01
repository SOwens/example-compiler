type op = 
  | Plus
  | Minus
  | Times
  | Div
  | Lt
  | Eq
  | And
  | Or
      [@@deriving show]

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

type tok_loc = (token * int) list
    [@@deriving show]

val lex : string -> int -> int -> (token * int) list
