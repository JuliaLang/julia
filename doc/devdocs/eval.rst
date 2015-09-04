.. currentmodule:: Base



******************
Eval of Julia code
******************

One of the hardest parts about learning how the Julia Language runs code is learning
how all of the pieces work together to execute a block of code.

Each chunk of code typically makes a trip through many esoteric acronyms such as (in no particular order),
`flisp`, `AST`, `C++`, `LLVM`, `eval`, `typeinf`, `macroexpand`, `sysimg` (or `system image`), `bootstrapping`,
`compile`, `parse`, `execute`, `JIT`, `interpret`, `box`, `unbox`, `intrinsic function`, `primitive function`
before turning into the desired result (hopefully).

Julia Execution
---------------

The 10,000 foot view of the whole process is as follows:

.. sidebar:: Definitions

   REPL
     REPL stands for Read-Eval-Print Loop.
     It's just what we call the command line environment for short.

   AST
     Abstract Syntax Tree
     The AST is the digital representation of the code structure.
     In this form the code has been tokenized for meaning
     so that it is more suitable for manipulation and execution.

1. The user starts `julia`.
2. The C function :c:func:`main` from `ui/repl.c` gets called.
   This function processes the command line arguments, filling in the :c:type:`jl_compileropts` struct and setting the variable :code:`ARGS`.
   It then initializes Julia (by calling `julia_init in task.c <https://github.com/JuliaLang/julia/blob/master/src/task.c>`_,
   which may load a previously compiled sysimg_).
   Finally, it passes off control to Julia by calling `Base._start() <https://github.com/JuliaLang/julia/blob/master/base/client.jl>`_.
#. When `_start()` takes over control, the subsequent sequence of commands depends on the command line arguments given.
   For example, if a filename was supplied, it will proceed to execute that file. Otherwise, it will start an interactive REPL.
#. Skipping the details about how the REPL interacts with the user,
   let's just say the program ends up with a block of code that it wants to run.
#. If the block of code to run is in a file, `jl_load(char *filename) <https://github.com/JuliaLang/julia/blob/master/src/toplevel.c>`_
   gets invoked to load the file and parse_ it. Each fragment of code is then passed to `eval` to execute.
#. Each fragment of code (or AST), is handed off to :func:`eval` to turn into results.
#. :func:`eval` takes each code fragment and tries to run it in `jl_toplevel_eval_flex() <https://github.com/JuliaLang/julia/blob/master/src/toplevel.c>`_.
#. :c:func:`jl_toplevel_eval_flex` decides whether the code is a "toplevel" action (such as `using` or `module`), which would be invalid inside a function.
   If so, it passes off the code to the toplevel interpreter.
#. :c:func:`jl_toplevel_eval_flex` then expands_ the code to eliminate any macros and to "lower" the AST to make it simpler to execute.
#. :c:func:`jl_toplevel_eval_flex` then uses some simple heuristics to decide whether to JIT compiler the AST or to interpret it directly.
#. The bulk of the work to interpret code is handled by `eval in interpreter.c <https://github.com/JuliaLang/julia/blob/master/src/interpreter.c>`_.
#. If instead, the code is compiled, the bulk of the work is handled by `codegen.cpp`.
   Whenever a Julia function is called for the first time with a given set of argument types, `type inference`_ will be run on that function.
   This information is used by the codegen_ step to generate faster code.
#. Eventually, the user quits the REPL, or the end of the program is reached, and the :func:`_start` method returns.
#. Just before exiting, :c:func:`main` calls `jl_atexit_hook() <https://github.com/JuliaLang/julia/blob/master/src/init.c>`_.
   This calls :func:`Base._atexit` (which calls any functions registered to :func:`atexit` inside Julia).
   Then it calls `jl_gc_run_all_finalizers() <https://github.com/JuliaLang/julia/blob/master/src/gc.c>`_.
   Finally, it gracefully cleans up all ``libuv`` handles and waits for them to flush and close.

.. _parse:

Parsing
-------

The Julia parser is a small lisp program written in femtolisp,
the source-code for which is distributed inside Julia in `src/flisp <https://github.com/JuliaLang/julia/tree/master/src/flisp>`_.

The interface functions for this are primarily defined in `jlfrontend.scm <https://github.com/JuliaLang/julia/blob/master/src/jlfrontend.scm>`_.
The code in `ast.c <https://github.com/JuliaLang/julia/blob/master/src/ast.c>`_ handles this handoff on the Julia side.

The other relevant files at this stage are `julia-parser.scm <https://github.com/JuliaLang/julia/blob/master/src/julia-parser.scm>`_,
which handles tokenizing Julia code and turning it into an AST,
and `julia-syntax.scm <https://github.com/JuliaLang/julia/blob/master/src/julia-syntax.scm>`_,
which handles transforming complex AST representations into simpler, "lowered" AST representations which are more suitable for analysis and execution.

.. _expands:

Macro Expansion
---------------

When :func:`eval` encounters a macro, it expands that AST node before attempting to evaluate the expression.
Macro expansion involves a handoff from :func:`eval` (in Julia), to the parser function :c:func:`jl_macroexpand` (written in `flisp`)
to the Julia macro itself (written in - what else - `Julia`) via :c:func:`fl_invoke_julia_macro`, and back.

Typically, macro expansion is invoked as a first step during a call to :func:`expand`/:c:func:`jl_expand`,
although it can also be invoked directly by a call to :func:`macroexpand`/:c:func:`jl_macroexpand`.

.. _type inference:

Type Inference
--------------

Type inference is implemented in Julia by `typeinf() in inference.jl <https://github.com/JuliaLang/julia/blob/master/base/inference.jl>`_.
Type inference is the process of examining a Julia function and determining bounds for the types of each of its variables,
as well as bounds on the type of the return value from the function.
This enables many future optimizations, such as unboxing of known immutable values,
and compile-time hoisting of various run-time operations such as computing field offsets and function pointers.
Type inference may also include other steps such as constant propagation and inlining.

.. _codegen:

JIT Code Generation
-------------------

.. sidebar:: More Definitions

    JIT
      Just-In-Time Compilation
      The process of generating native-machine code into memory right when it is needed.

    LLVM
      Low-Level Virtual Machine (a compiler)
      The Julia JIT compiler is a program/library called libLLVM.
      Codegen in Julia refers both to the process of taking a Julia AST and turning it into LLVM instructions,
      and the process of LLVM optimizing that and turning it into native assembly instructions.

    C++
      The programming language that LLVM is implemented in,
      which means that codegen is also implemented in this language.
      The rest of Julia's library is implemented in C,
      in part because it's smaller feature set makes it more usable as a cross-language interface layer.

    box
      This term is used to describe the process of taking a value and allocating a wrapper around the data
      that is tracked by the garbage collector (gc) and is tagged with the object's type.

    unbox
      The reverse of boxing a value. This operation enables more efficient manipulation of data
      when the type of that data is fully known at compile-time (through type inference).

    generic function
      A Julia function composed of multiple "methods" that are selected for dynamic dispatch based on the argument type-signature

    anonymous function or "method"
      A Julia function without a name and without type-dispatch capabilities

    primitive function
      A function implemented in C but exposed in Julia as a named function "method"
      (albeit without generic function dispatch capabilities, similar to a anonymous function)

    intrinsic function
      A low-level operation exposed as a function in Julia.
      These pseudo-functions implement operations on raw bits such as add and sign extend
      that cannot be expressed directly in any other way.
      Since they operate on bits directly, they must be compiled into a function
      and surrounded by a call to `Core.Intrinsics.box(T, ...)` to reassign type information to the value.

Codegen is the process of turning a Julia AST into native machine code.

The JIT environment is initialized by an early call to `jl_init_codegen in codegen.cpp <https://github.com/JuliaLang/julia/blob/master/src/codegen.cpp>`_.

On demand, a Julia method is converted into a native function by the function :c:func:`emit_function(jl_lambda_info_t*) <emit_function>`.
(note, when using the MCJIT (in LLVM v3.4+), each function must be JIT into a new module.)
This function recursively calls :c:func:`emit_expr` until the entire function has been emitted.

Much of the remaining bulk of this file is devoted to various manual optimizations of specific code patterns.
For example, :c:func:`emit_known_call` knows how to inline many of the primitive functions
(defined in `builtins.c <https://github.com/JuliaLang/julia/blob/master/src/builtins.c>`_) for various combinations of argument types.

Other parts of codegen are handled by various helper files:

`debuginfo.cpp <https://github.com/JuliaLang/julia/blob/master/src/debuginfo.cpp>`_
  Handles backtraces for JIT functions

`ccall.cpp <https://github.com/JuliaLang/julia/blob/master/src/ccall.cpp>`_
  Handles the ccall and llvmcall FFI, along with various `abi_*.cpp` files

`intrinsics.cpp <https://github.com/JuliaLang/julia/blob/master/src/intrinsics.cpp>`_
  Handles the emission of various low-level intrinsic functions

.. _sysimg:

System Image
------------

.. sidebar:: Bootstrapping

    The process of creating a new system image is called "bootstrapping".

    The etymology of this word comes from the phrase "pulling one's self up by the bootstraps",
    and refers to the idea of starting from a very limited set of available functions and definitions
    and ending with the creation of a full-featured environment.

The system image is a precompiled archive of a set of Julia files.
The `sys.ji` file distributed with Julia is one such system image,
generated by executing the file `sysimg.jl <https://github.com/JuliaLang/julia/blob/master/base/sysimg.jl>`_,
and serializing the resulting environment (including Types, Functions, Modules, and all other defined values)
into a file. Therefore, it contains a frozen version of the :mod:`Main`, :mod:`Core`, and :mod:`Base` modules (and whatever else was in the environment at the end of bootstrapping).
This serializer/deserializer is implemented by `jl_save_system_image/jl_restore_system_image in dump.c <https://github.com/JuliaLang/julia/blob/master/src/dump.c>`_.

If there is no sysimg file (:code:`jl_compileropts.image_file == NULL`),
this also implies that `--build` was given on the command line,
so the final result should be a new sysimg file.
During Julia initialization, minimal :mod:`Core` and :mod:`Main` modules are created.
Then a file named ``boot.jl`` is evaluated from the current directory.
Julia then evaluates any file given as a command line argument until it reaches the end.
Finally, it saves the resulting environment to a "sysimg" file for use as a starting point for a future Julia run.
