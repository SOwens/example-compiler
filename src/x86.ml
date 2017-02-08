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

(* A fragment of x86-64 AST, and printing in NASM syntax *)
(* Translated from HOL4 in examples/machine-code/instruction-set-models/x86_64 *)

open Format
open Util

(* 64-bit registers *)
type reg =
  | RAX | RBX | RCX  | RDX  | RSP  | RBP  | RSI  | RDI
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

let show_reg r =
  match r with
  | RAX -> "rax"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let pp_reg fmt r =
  fprintf fmt "%s" (show_reg r)

type byte_reg = B of reg

let show_byte_reg (B r) =
  match r with
  | RAX -> "al"
  | RBX -> "bl"
  | RCX -> "cl"
  | RDX -> "dl"
  | RSP -> "spl"
  | RBP -> "bpl"
  | RSI -> "sil"
  | RDI -> "dil"
  | R8 -> "r8b"
  | R9 -> "r9b"
  | R10 -> "r10b"
  | R11 -> "r11b"
  | R12 -> "r12b"
  | R13 -> "r13b"
  | R14 -> "r14b"
  | R15 -> "r15b"

let pp_byte_reg fmt r =
  fprintf fmt "%s" (show_byte_reg r)

type address_component =
  | EAScaled of int * reg
  | EAReg of reg
  | EAConst of int64

type displacement =
  | Concrete_disp of int64
  | Label_disp of string

type rm =
  | Zr of reg                                          (* register *)
  (* Zm (Some (scale, index), Some base, Some displacement) =
     mem[scale * index + base + displacement] *)
  | Zm of (int * reg) option * reg option * displacement option

let rec simple_add_exp fmt l =
  match l with
  | [] -> ()
  | [Some x] -> fprintf fmt "%s" x
  | [None] -> fprintf fmt "0"
  | Some x :: y -> fprintf fmt "%s + %a" x simple_add_exp y
  | None :: y -> simple_add_exp fmt y

let show_displacement d =
  match d with
  | Concrete_disp i -> Int64.to_string i
  | Label_disp l -> l ^ "_Global"

let pp_rm fmt rm =
  match rm with
  | Zr r -> fprintf fmt "%a" pp_reg r
  | Zm (idx,base,disp) ->
    fprintf fmt "qword [%a]"
      simple_add_exp [option_map (fun (scale,i) -> show_reg i ^ " * " ^ string_of_int scale) idx;
                      option_map show_reg base;
                      option_map show_displacement disp]

type dest_src =
  | Zrm_i of rm  * int64  (* mnemonic r/mXX, immXX (sign-extended) *)
  | Zrm_r of rm  * reg    (* mnemonic r/mXX, rXX *)
  | Zr_rm of reg * rm     (* mnemonic rXX, r/mXX *)

let pp_dest_src fmt ds =
  match ds with
  | Zrm_i (rm, i) ->
    fprintf fmt "%a, %Ld"
      pp_rm rm
      i
  | Zrm_r (rm, reg) ->
    fprintf fmt "%a, %a"
      pp_rm rm
      pp_reg reg
  | Zr_rm (reg, rm) ->
    fprintf fmt "%a, %a"
      pp_reg reg
      pp_rm rm

type imm_rm =
  | Zi_rm of rm      (* r/mXX *)
  | Zi    of int64   (* sign-extended immediate *)

let pp_imm_rm fmt ir =
  match ir with
  | Zi_rm rm -> pp_rm fmt rm
  | Zi i -> fprintf fmt "%Ld" i

type binop_name =
  | Zadc | Zadd | Zand | Zcmp | Zor | Zshl | Zshr
  | Zsar | Zsub | Zsbb | Ztest | Zxor

let show_binop_name b =
  match b with
  | Zadc -> "adc"
  | Zadd -> "add"
  | Zand -> "and"
  | Zcmp -> "cmp"
  | Zor -> "or"
  | Zshl -> "shl"
  | Zshr -> "shr"
  | Zsar -> "sar"
  | Zsub -> "sub"
  | Zsbb -> "sbb"
  | Ztest -> "test"
  | Zxor -> "xor"

let pp_binop_name fmt b =
  fprintf fmt "%s" (show_binop_name b)

type monop_name = Zdec | Zinc | Znot | Zneg

let show_monop_name b =
  match b with
  | Zdec -> "dec"
  | Zinc -> "inc"
  | Znot -> "not"
  | Zneg -> "neg"

let pp_monop_name fmt b =
  fprintf fmt "%s" (show_monop_name b)

type cond = (* this list is not complete *)
  | Z_ALWAYS            (* N = not     *)
  | Z_E | Z_NE          (* E = equal   *)
  | Z_S | Z_NS          (* S = signed  *)
  | Z_A | Z_NA          (* A = above   *)
  | Z_B | Z_NB          (* B = below   *)
  | Z_L | Z_NL          (* L = less *)
  | Z_G | Z_NG          (* L = greater *)

let pp_cond fmt c =
  fprintf fmt "%s"
    (match c with
     | Z_ALWAYS -> "mp"
     | Z_E -> "e"
     | Z_NE -> "ne"
     | Z_S -> "s"
     | Z_NS -> "ns"
     | Z_A -> "a"
     | Z_NA -> "na"
     | Z_B -> "b"
     | Z_NB -> "nb"
     | Z_L -> "l"
     | Z_NL -> "nl"
     | Z_G -> "g"
     | Z_NG -> "ng")

type instruction =
  | Zlabel     of string
  | Zbinop     of binop_name * dest_src
  | Zmonop     of monop_name * rm
(*
  | Zcmpxchg   of rm * reg
  | Zxadd      of rm * reg
  | Zxchg      of rm * reg
   *)
  (* either reg := reg * rm; or reg := rm * int *)
  | Zimul      of reg * rm * int64 option
  | Zidiv      of rm (* RAX := RDX,RAX / rm; RDX := RDX,RAX mod rm *)
  | Zlea       of dest_src
  | Zpop       of rm
  | Zpush      of imm_rm
  | Zcall      of string
  | Zret
  | Zleave
  | Zcpuid
  | Zmov       of dest_src
  (* | Zmovzx     of dest_src *)
  (* jcc includes jmp rel, i.e. unconditional relative jumps. *)
  | Zjcc       of cond * string
  | Zjmp       of rm                (* jmp excludes relative jumps, see jcc. *)
  | Zset       of cond * byte_reg   (* Set operates on a byte of memory or
                                       register. Here, we'll only use registers
                                    *)

let pp_instruction fmt i =
  match i with
  | Zlabel s ->
    fprintf fmt "%s:" s
  | Zbinop (n, ds) ->
    fprintf fmt "%a %a"
      pp_binop_name n
      pp_dest_src ds
  | Zmonop (n, rm) ->
    fprintf fmt "%a %a"
      pp_monop_name n
      pp_rm rm
  | Zimul (r1, rm2, None) ->
    (* r1 := r1 * rm2 *)
    fprintf fmt "imul %a, %a"
      pp_reg r1
      pp_rm rm2
  | Zimul (r1, rm2, Some i) ->
    (* r1 := rm2 * i *)
    fprintf fmt "imul %a, %a, %Ld"
      pp_reg r1
      pp_rm rm2
      i
  | Zidiv rm ->
    fprintf fmt "idiv %a"
      pp_rm rm
  | Zlea ds ->
    fprintf fmt "lea %a"
      pp_dest_src ds
  | Zpop rm ->
    fprintf fmt "pop %a"
      pp_rm rm
  | Zpush ir ->
    fprintf fmt "push %a"
      pp_imm_rm ir
  | Zcall lab ->
    fprintf fmt "call %s" lab
  | Zret ->
    fprintf fmt "ret"
  | Zleave ->
    fprintf fmt "leave"
  | Zcpuid ->
    fprintf fmt "cpuid"
  | Zmov ds ->
    fprintf fmt "mov %a"
      pp_dest_src ds
  | Zjcc (c, lab) ->
    fprintf fmt "j%a %s"
      pp_cond c
      lab
  | Zjmp rm ->
    fprintf fmt "jmp %a"
      pp_rm rm
  | Zset (cond, reg) ->
    fprintf fmt "set%a %a"
      pp_cond cond
      pp_byte_reg reg

let pp_instr_list fmt il =
  List.iter
    (fun i ->
       match i with
       | Zlabel _ -> fprintf fmt "%a@\n" pp_instruction i
       | _ -> fprintf fmt "  %a@\n" pp_instruction i)
    il
