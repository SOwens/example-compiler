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

(* Driver for interpreter executable. *)

let filename = ref None;;

let usage_msg =
  "example interpreter \nexample usage:    " ^ Sys.argv.(0)^ " test.expl\n";;

let _ =
  Arg.parse []
    (fun s ->
       match !filename with
       | None ->
         filename := Some s
       | Some s' ->
         (Format.printf "Error: given multiple files to run: %s and %s\n" s' s;
          exit 1))
    usage_msg;;

let _ =
  match !filename with
  | None ->
    (print_string usage_msg;
     exit 1)
  | Some filename ->
    AstInterp.interp_prog (FrontEnd.front_end filename false)
