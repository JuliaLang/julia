                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |

Compiling
---------

Required build tools: gcc, g++, gfortran, curl (to download third party tool source), GNU make.

Supported platforms: x86 and x86/64 GNU/Linux; x86/64 OS X.

To compile:

- Run `make` in `external/` to download and build external dependencies.
- Run `make` in the top-level directory to build julia.

No installation is necessary — julia can be run from the directory where it was built. You might want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.


Directories
-----------

    contrib/       emacs and textmate support for julia
    doc/           TeX manual and other notes
    external/      external dependencies
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    ui/            source for various frontends
    test/          some tests


Emacs Setup
-----------

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.


Brief Introduction
------------------

Julia is a very high level dynamic language with good performance, implemented as a JIT compiler using the LLVM toolchain. Julia is multi-paradigm, combining features of functional, object-oriented, and imperative languages. The primary unifying feature is multiple dispatch: defining functions on different combinations of datatypes, and defining new datatypes. Operators are functions. This model fits mathematical programming well, where it is most common to define new functions on existing datatypes, and it is unnatural for only one argument to "own" an operation (as in typical single-dispatch OO languages). The compiler uses type inference to eliminate dispatch overhead where possible.

Advantages of julia over other comparable systems are as follows:

- Free and open source
- Consistent and powerful generic function model
- High performance just-in-time compilation and execution
- User-defined types are just as fast as built-in types
- Minimal syntactic overhead to define new types
- High-level constructs for parallelism
- Multi-dimensional array comprehensions
- Lightweight "green" threading (coroutines)
- A powerful but unobstrusive type system (optional typing)
- Elegant and extensible automatic conversions of numeric types
- Lisp-like macros and other powerful metaprogramming facilities
- Efficient support for Unicode, including but not limited to UTF-8
- Powerful shell-like capabilities for managing other processes


Rationale
---------

Scientific computing has traditionally required the highest performance, yet domain experts have largely moved to slower dynamic languages for daily work. We believe there are many good reasons to prefer dynamic languages for these applications, and we don't expect their use to diminish any time soon. Fortunately, modern language design and compiler techniques make it possible to mostly eliminate the performance trade-off and provide a single environment productive enough for prototyping and performant enough for deploying applications. However, an open-source language with these characteristics has not emerged. Our project, Julia, fills this gap.

MATLAB generally dominates the field of programming languages for the applied sciences. We take note of some of the reasons for its success: it is well-suited to linear algebra and array manipulation, easy to use, and achieves impressive performance due to a JIT compiler (introduced in 2003). Several open source systems (Python/NumPy, R, Octave, SciLab) offer comparable advantages, but in particular have not kept up in the last category.

Closing this performance gap is a primary goal of ours, but at the same time we feel there is room to increase power, flexibility, and usability for general programming. The rising importance of parallel computing and cloud computing provides further motivation to design a new system keeping those concerns in mind from the beginning.
