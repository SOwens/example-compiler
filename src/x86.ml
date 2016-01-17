open Format
open Util

(* Translated from HOL4 in examples/machine-code/instruction-set-models/x86_64 *)

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

type rm = 
  | Zr of reg                                          (* register *)
  | Zm of (int * reg) option * reg option * Int64.t option (* mem[2^{scale} * index + base + displacement] *)

let rec simple_add_exp fmt l =
  match l with
  | [] -> ()
  | [Some x] -> fprintf fmt "%s" x
  | [None] -> ()
  | Some x :: y -> fprintf fmt "%s + %a" x simple_add_exp y
  | None :: y -> simple_add_exp fmt y

let pp_rm fmt rm =
  match rm with
  | Zr r -> fprintf fmt "%a" pp_reg r
  | Zm (idx,base,disp) -> 
    fprintf fmt "[%a]"
      simple_add_exp [option_map (fun (scale,i) -> show_reg i ^ " * " ^ string_of_int scale) idx; 
                      option_map show_reg base; 
                      option_map [%show : Int64.t] disp]

type dest_src = 
  | Zrm_i of rm  * Int64.t  (* mnemonic r/mXX, immXX (sign-extended) *)
  | Zrm_r of rm  * reg    (* mnemonic r/mXX, rXX *)
  | Zr_rm of reg * rm     (* mnemonic rXX, r/mXX *)

let pp_dest_src fmt ds =
  match ds with
  | Zrm_i (rm, i) ->
    fprintf fmt "%a, %s"
      pp_rm rm
      ([%show : Int64.t] i)
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
  | Zi    of Int64.t   (* sign-extended immediate *) 

let pp_imm_rm fmt ir = 
  match ir with
  | Zi_rm rm -> pp_rm fmt rm
  | Zi i -> fprintf fmt "%s" ([%show:Int64.t] i)

type binop_name = Zadc | Zadd | Zand | Zcmp | Zor | Zshl | Zshr | Zsar | Zsub | Zsbb | Ztest | Zxor

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

let pp_cond fmt c =
  match c with
  | Z_ALWAYS -> fprintf fmt "mp"
  | Z_E -> fprintf fmt "e"
  | Z_NE -> fprintf fmt "ne"
  | Z_S -> fprintf fmt "s"
  | Z_NS -> fprintf fmt "ns"
  | Z_A -> fprintf fmt "s"
  | Z_NA -> fprintf fmt "ns"
  | Z_B -> fprintf fmt "s"
  | Z_NB -> fprintf fmt "ns"

type instruction = 
  | Zbinop     of binop_name * dest_src
  | Zmonop     of monop_name * rm
(*
  | Zcmpxchg   of rm * reg
  | Zxadd      of rm * reg
  | Zxchg      of rm * reg
   *)
  | Zimul       of rm * rm * Int64.t option
  | Zdiv       of rm (* RAX := RDX,RAX / rm; RDX := RDX,RAX mod rm *)
  | Zlea       of dest_src
  | Zpop       of rm
  | Zpush      of imm_rm
  | Zcall      of imm_rm
  | Zret       of Int64.t
  | Zcpuid
  | Zmov       of dest_src
  (* | Zmovzx     of dest_src *)
  | Zjcc       of cond * string     (* jcc includes jmp rel, i.e. unconditional relative jumps. *)
  | Zjmp       of rm                (* jmp excludes relative jumps, see jcc. *)
  (* | Zloop      of cond * Int64.t    (* Here Zcond over approximates possibilities. *) *)

let pp_instruction fmt i = 
  match i with
  | Zbinop (n, ds) ->
    fprintf fmt "%a %a"
      pp_binop_name n
      pp_dest_src ds
  | Zmonop (n, rm) ->
    fprintf fmt "%a %a"
      pp_monop_name n
      pp_rm rm
  | Zimul (rm1, rm2, None) ->
    fprintf fmt "imul %a, %a"
      pp_rm rm1
      pp_rm rm2
  | Zimul (rm1, rm2, Some i) ->
    fprintf fmt "imul %a, %a, %s"
      pp_rm rm1
      pp_rm rm2
      ([%show:Int64.t] i)
  | Zdiv rm ->
    fprintf fmt "div %a"
      pp_rm rm
  | Zlea ds ->
    fprintf fmt "lea %a"
      pp_dest_src ds
  | Zpop rm ->
    fprintf fmt "pop %a"
      pp_rm rm
  | Zpush ir ->
    fprintf fmt "pop %a"
      pp_imm_rm ir
  | Zcall ir ->
    fprintf fmt "pop %a"
      pp_imm_rm ir
  | Zret i ->
    fprintf fmt "ret %s" ([%show : Int64.t] i)
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
