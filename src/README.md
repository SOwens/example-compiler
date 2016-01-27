Compiler structure
------------------

The top-level files are _compile.ml_ and _interp.ml_, driving a comiler and
interpreter respectively.

Front end
---------

The front-end in _frontEnd.ml_ packages a lexer (_tokens.ml_), parser
(_sourceAst.ml_), and type checker (_typeCheck.ml_) and produces an AST. It is
shared by the compiler and interpreter.

AST transformations
-------------------

The AST goes through a series of transformations

- _constProp.ml_: Constant propagation and folding
  The compiler later relies on there being no operators with two constant
  operands.
TODO: Check below here
- _unnestExp.ml_: Un-nesting of expressions
  Flattens out expressions, using assignments to temporary variables. Ensures
  that no expressions contain sub-expressions with operators in them.
- _blockStructure.ml_: Transforms then AST to a control-flow graph of basic
  blocks. Requires expressions to be un-nested and booleans to be removed.

Basic-block transformations
---------------------------

- _shrinkImmediates.ml_: Removes all constants that don't fit into 32 bits
  (since that's the maximum size of an immediate on x86--64), possibly
  introducing assignments to temporary variables .
- _liveVarAnalysis.ml_: Performs live variable analysis, and removes unused
  assignments.
  (* TODO: Add check that operators are pure *)
- _regAlloc.ml_: Perform register allocation. This changes all identifiers from
  named variables to registers and stack offsets.
- _linearistCFG.ml_: Flatten the CFG, introducing labels and branches.

Backend
-------
- _x86.ml_: An AST for the small subset of x86-64 that the compiler generates.
  Incudes a printer for it in NASM syntax.
- _instrSelX86.ml_: Generate x86 code.

Misc
----
- _util.ml_: Misc. utility functions.
