(*
 * Example compiler
 * Copyright (C) 2015-2016 Scott Owens
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

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

let option_map f x =
  match x with
  | None -> None
  | Some y -> Some (f y)
