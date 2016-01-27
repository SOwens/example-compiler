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

(* Flatten expressions so that they follow this grammar. We don't introduce a
   new type, but do define predicates on the SourceAst.exp type. 

   type ae =
   | Num of int64
   | Bool of bool
   | Ident of SourceAst.id

   type flat_exp =
   | Num of int64
   | Bool of bool
   | Ident of SourceAst.id * ae list
   | Op of ae * op * ae
   | Uop of ae
   | Array of ae list
 *)

open SourceAst
val unnest : stmt list -> stmt list
val is_atomic : exp -> bool
val is_flat : exp -> bool
