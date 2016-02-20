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
*nasm* syntax. The compiler has an optional argument `-osx` to generate OS X
compatible assembly. Otherwise it generates Linux compatible assembly.

Compiling target programs
-------------------------

First run `make` in the `runtime` directory to compile the very simple runtime
library (using gcc).

Use `nasm -f macho64 FILENAME.s` to assemble the compiler's output for
*FILENAME*, and then `gcc COMPILER_DIR/runtime/io.o FILENAME.o -o FILENAME`
to link the program with the runtime library.

See the `tests` directory for some example programs. NB, the `Makefile` in that
directory must be edited to specialise it for either OS X or Linux.

The source language
-------------------

Keywords are `+ - * / | & << < > = || && ! := do while if then else input output true false array`

Identifiers are strings of letters, underscores, and digits (not starting with
a digit) that are not keywords.

Numbers are sequences of digits that fit into a 64-bit signed integer.

Comments start with '//' and last until the end of the line.

All numerical operations are on signed, 2s complement 64-bit integers.

Multi-dimensional arrays are supported. Array elements are 64-bit integers.

Variables aren't separately declared, but a variable is only in scope inside of
the curly braces that it was first assigned in.

op ::=  
| `+`  --- Addition  
| `-`  --- Subtraction  
| `*`  --- Multiplication  
| `/`  --- Division  
| `|`  --- Bitwise or  
| `&`  --- Bitwise and  
| `<<` --- Left shift  
| `<`  --- Less than  
| `>`  --- Greater than  
| `=`  --- Equality  
| `||` --- Logical or  
| `&&` --- Logical and

uop ::=  
| `!`  --- Logical negation  
| `-`  --- Unary minus

indices ::=  
| `[` exp `]` indices  
| epsilon

atomic_exp ::=  
| identifier indices  
| number  
| `true`  
| `false`  
| uop atomic_exp  
| `array` indices  
| `(` exp `)`

exp ::=  
| atomic_exp op atomic_exp  
| atomic_exp

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

Loading the compiler in utop
----------------------------

First build the compiler, by running make from the src directory. Then load the
packages that the compiler uses with the `#require` command:

```
#require "ppx_deriving";;
#require "ppx_deriving.ord";;
#require "ppx_deriving.show";;
#require "extlib";;
#require "str";;
```

You can add these 3 lines to the `.ocamlinit` file in your home directory, so
that you don't have to manually enter them each time you start a new utop
session. The contents of `.ocamlinit` are run each time you start a new utop.

The OCaml compilation manager stores all of the compiled OCaml sources in the
`_build` directory, with the extension `.cmo`. The following tells utop to look
there for source files.
```
#directory "_build";;
```

To load a particular module, for example, LineariseCfg, use the `#load_rec` command.
```
#load_rec "lineariseCfg.cmo";;
```

Often a good way to work on a file is to `#load_rec` all of the modules that it
depends on, and then `#use` the file.
