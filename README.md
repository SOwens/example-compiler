A simple compiler for a simple imperative language targeting x86-64 assembly
(for systems that follow the System V ABI, so most operating systems **except**
Windows).

Running the compiler
--------------------

The compiler relies on OCaml and several libraries: ocamlfind, extlib,
ppx_deriving, and ppx_monadic. It is tested with OCaml version 4.02.3, and
ocamlfind 1.5.6, extlib 1.7.0, ppx_deriving 3.0, and ppx_monadic 1.0.3. All of
these libraries can be installed via opam. Opam installs everything into
`.opam` in your home directory:
```
opam install extlib
opam install ppx_deriving
opam install ppx_monadic
```

To compile the compiler run `make` in the `src` directory. This should produce
`compile.byte` and `interp.byte` executables.  Both take a single command-line
argument: a source file name with the `.expl` extension. `interp.byte` runs the
file, `compile.byte` compiles it, generating an x86-46 assembly `.s` file in
*nasm* syntax.

Compiling target programs
-------------------------

First run `make` in the `runtime` directory to compile the very simple runtime
library (using gcc).

Use `nasm -f macho64 FILENAME.s` to assemble the compiler's output for
*FILENAME*, and then `gcc COMPILER_DIR/runtime/io.o FILENAME.o -o FILENAME`
to link the program with the runtime library.

The source language
-------------------

Keywords are `+ - * / | & << < > = || && := do while if then else input output true false array`

Identifiers are strings of letters and digits (starting with a letter) that
are not keywords.

Numbers are sequences of digits that fit into a 64-bit signed integer.

All numerical operations are on signed, 2s complement 64-bit integers.

Multi-dimensional arrays are supported. Array elements are 64-bit integers.

Variables aren't separately declared, but a variable is only in scope inside of
the curly braces that it was first assigned in.

op ::=  
| `+`  --- Addition  
| `-`  --- Subtraction  
| `*`  --- Multiplication  
| `/`  --- Division  
| `|`  --- Bitwise-or  
| `&`  --- Bitwise-and  
| `<<` --- Left shift  
| `<`  --- Less than  
| `>`  --- Greater than  
| `=`  --- Equality  
| `||` --- Logical Or  
| `&&` --- Logical And

indices ::=  
| epsilon  
| `[` exp `]` indices

atomic_exp ::=  
| identifier indices  
| number  
| `true`  
| `false`  
| `(` atomic_exp op atomic_exp `)`
| `array` indices

exp ::=  
| atomic_exp
| atomic_exp op atmoic_exp

stmt ::=  
| identifier indices `:=` exp  
| `while` exp stmt  
| `do` stmt `while` exp  
| `if` exp `then` stmt `else` stmt  
| `{` stmts `}`  
| `input` identifier  
| `output` identifier

stmts ::=  
| epsilon  
| stmt stmts

A program is just a stmts.
