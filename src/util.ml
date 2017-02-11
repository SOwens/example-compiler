(*
 * Example compiler
 * Copyright (C) 2015-2017 Scott Owens
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

(* General utility functions *)

(* For lex/parse/type errors *)
exception BadInput of string
(* For cases that shouldn't be able to happen in the compiler *)

exception InternalError of string

(* Maps with string keys *)
module Strmap = Map.Make(String)

(* Maps with integer keys *)
module Intmap =
  Map.Make(struct type t = int let (compare : int -> int -> int) = compare end)

(* Build the list [0, ..., n-1] *)
let count (n : int) : int list =
  let rec f n next =
    if n <= 0 then
      []
    else
      next :: f (n - 1) (next + 1)
  in
  f n 0

(* Combine 2 lists into a list of pairs, truncating the longer list *)
let rec zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match (l1,l2) with
  | (h1::t1, h2::t2) -> (h1,h2) :: zip t1 t2
  | _ -> []

(* Convert a list to a ; separated string *)
let show_list' (d1 : string) (d2 : string) (show : 'a -> string) (l : 'a list)
  : string =
  let rec f l =
    match l with
    | [] -> ""
    | [h] -> show h
    | (h::t) ->
      show h ^ "; " ^ f t
  in
    d1 ^ f l ^ d2

let show_list show = show_list' "[" "]" show
let show_set show = show_list' "{" "}" show

(* Pretty-pring a list, separated by ; *)
let pp_list' (d1 : string) (d2 : string) (pp : Format.formatter -> 'a -> unit)
    (fmt : Format.formatter) (l : 'a list)
  : unit =
  let rec f fmt l =
    match l with
    | [] -> ()
    | [h] -> pp fmt h
    | (h::t) ->
      Format.fprintf fmt "%a;@ %a"
        pp h
        f t
  in
    Format.fprintf fmt "%s@[%a@]%s" d1 f l d2

let pp_list pp = pp_list' "[" "]" pp
let pp_set pp = pp_list' "{" "}" pp

let option_map (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with
  | None -> None
  | Some y -> Some (f y)

(* Compare two options, where None is less than Some *)
let option_compare (c : 'a -> 'b -> int) (o1 : 'a option) (o2 :'b option)
  : int =
  match (o1, o2) with
  | (None, None) -> 0
  | (Some _, None) -> 1
  | (None, Some _) -> -1
  | (Some x, Some y) -> c x y

module Std = struct
  (* Copy and paste from extlib to get input_file without making a dependency.
     extlib is LGPL 2.1, and so this sub-module is too.
     https://github.com/ygrek/ocaml-extlib/blob/33f744ddb28d6a0f4c96832145e1a6e384644709/src/std.ml *)

  let finally handler f x =
    let r = (
      try
        f x
      with
        e -> handler (); raise e
    ) in
    handler ();
    r

  let buf_len = 8192

  let input_all ic =
    let rec loop acc total buf ofs =
      let n = input ic buf ofs (buf_len - ofs) in
      if n = 0 then
        let res = Bytes.create total in
        let pos = total - ofs in
        let _ = Bytes.blit buf 0 res pos ofs in
        let coll pos buf =
          let new_pos = pos - buf_len in
          Bytes.blit buf 0 res new_pos buf_len;
          new_pos in
        let _ = List.fold_left coll pos acc in
        (* [res] doesn't escape and will not be mutated again *)
        Bytes.unsafe_to_string res
      else
        let new_ofs = ofs + n in
        let new_total = total + n in
        if new_ofs = buf_len then
          loop (buf :: acc) new_total (Bytes.create buf_len) 0
        else loop acc new_total buf new_ofs in
    loop [] 0 (Bytes.create buf_len) 0

  let input_file ?(bin=false) fname =
    let ch = (if bin then open_in_bin else open_in) fname in
    finally (fun () -> close_in ch) input_all ch

end
