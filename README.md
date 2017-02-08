A simple compiler for a simple imperative language targeting x86-64 assembly
(for systems that follow the System V ABI, so most operating systems **except**
Windows).

Running the compiler
--------------------

The compiler relies on OCaml and the ocamlfind library. It is tested with OCaml
versions 4.02.3 and 4.04.0, and ocamlfind 1.5.6 and 1.7.1. Ocamlfind can be
installed via opam. If you have installed anything via Opam (for example,
Merlin, or ocp-indent), ocamlfind is probably installed already. Opam installs
everything into `.opam` in your home directory:
```
opam install ocamlfind
```

To compile the compiler run `make` in the `src` directory. This should produce
`compile.byte` and `interp.byte` executables. Both take a single command-line
argument: a source file name with the `.expl` extension. `interp.byte` runs the
file, `compile.byte` compiles it, generating an x86-46 assembly `.s` file in
*nasm* syntax.

Compiling target programs
-------------------------

First run `make` in the `runtime` directory to compile the very simple runtime
library (using gcc).

On Linux, use `nasm -f elf64 FILENAME.s` to assemble the compiler's output for
*FILENAME*. On Mac, use `nasm -f macho64 --prefix _ FILENAME.s`. Then use `gcc
COMPILER_DIR/runtime/io.o FILENAME.o -o FILENAME` to link the program with the
runtime library.

See the `tests` directory for some example programs.

The source language
-------------------

Keywords are `+ - * / | & << < > = || && ! := do while if then else input output true false array return function let bool int`

Identifiers are strings of letters, underscores, and digits (not starting with
a digit) that are not keywords.

Numbers are sequences of digits that fit into a 64-bit signed integer.

Comments start with '//' and last until the end of the line.

All numerical operations are on signed, 2s complement 64-bit integers.

Multi-dimensional arrays are supported. Array elements are 64-bit integers.

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

args ::=  
| exp  
| exp `,` args

atomic_exp ::=  
| identifier indices --- Variable use and array indexing  
| identifier `(` args `)` --- Function call  
| number             --- Integer constant  
| `true`             --- Boolean constant  
| `false`            --- Boolean constant  
| uop atomic_exp     --- Unary operation  
| `array` indices    --- Array allocation  
| `(` exp `)`        --- Parenthesised expression

exp ::=  
| atomic_exp op atomic_exp --- Binary operation  
| atomic_exp

stmt ::=  
| identifier indices `:=` exp  
| `while` exp stmt  
| `do` stmt `while` exp  
| `if` exp `then` stmt `else` stmt  
| `{` stmts `}`  
| `input` identifier  
| `output` identifier  
| `return` identifier

stmts ::=  
| epsilon  
| stmt stmts

typ ::=  
| `int`          --- a 64-bit signed integer  
| `bool`         --- a boolean  
| `array` number --- an n dimensional array of 64-bit signed integers

params ::=  
| `(` identifier `:` type `)`  
|  `(` identifier `:` type `)` params

var_decs ::=  
| epsilon  
| `let` identifier `:` typ `=` exp var_decs

functions ::=  
| epsilon  
| `function` identifier params `:` typ `{` var_decs stmts `}` funcs

program ::=  
| var_decs functions

Loading the compiler in utop
----------------------------

First build the compiler, by running make from the src directory. In utop load
the packages that the compiler uses with the `#require` command:

```
#require "str";;
```

You can add this line to the `.ocamlinit` file in your home directory, so that
you don't have to manually enter it each time you start a new utop session.
The contents of `.ocamlinit` are run each time you start a new utop.

The OCaml compilation manager (ocamlbuild) stores all of the compiled OCaml
sources in the `_build` directory, with the extension `.cmo`. The following
tells utop to look there for source files.
```
#directory "_build";;
```

To load a particular module, for example, LineariseCfg, use the `#load_rec` command.
```
#load_rec "lineariseCfg.cmo";;
```

You can then open the module if you want (`open LineariseCfg`), or call
functions directly (`LineariseCfg.cfg_to_linear`). Loading a compiled module in
this way only gives you access to the values and functions that are exported in
the corresponding `.mli` file. Often a good way to work on a file is to
`#load_rec` all of the modules that it depends on, and then `#use` the file.
