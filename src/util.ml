exception BadInput of string
exception InternalError of string

module Strmap = Map.Make(String)

let count (n : int) : int list =
  let rec f n next = 
    if n <= 0 then
      []
    else
      next :: f (n - 1) (next + 1)
  in
  f n 0

let rec zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match (l1,l2) with
  | (h1::t1, h2::t2) -> (h1,h2) :: zip t1 t2
  | _ -> []

let pp_list' d1 d2 pp fmt l =
  let rec f fmt l =
    match l with
    | [] -> ()
    | [h] -> pp fmt h
    | (h::t) ->
      Format.fprintf fmt "%a;@ %a"
        pp h
        f t
  in
    Format.fprintf fmt "@[%s%a%s@]" d1 f l d2

let pp_list pp = pp_list' "[" "]" pp
let pp_set pp = pp_list' "{" "}" pp
