                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |


Brief Introduction
------------------

Julia is a very high level dynamic language with optional typing, multiple dispatch, and good performance, achieved by using just-in-time (JIT) compilation, implemented using LLVM. The language is multi-paradigm, combining features of functional, object-oriented, and imperative traditions.

Although the beginning programmer need not use multiple dispatch, it is one of the core unifying features of Julia: functions are defined on different combinations of argument types, and applied by dispatching to the most specific matching definition. This model is a good fit for mathematical programming, where it is unnatural for the first argument to "own" an operation as in traditional object-oriented dispatch. Operators are just functions with special notation — to extend addition to new user-defined data types, you define new methods for the `+` function.

Using type inference, Julia's compiler can often eliminate most or all of the dispatch overhead, thereby generating efficient code.

Some advantages of Julia over comparable systems include:

- Free and open source
- Consistent and powerful generic function model
- High performance just-in-time compilation and execution
- User-defined types are first-class: as fast as built-ins
- Simple syntax for defining new types
- High-level constructs for parallelism
- Multi-dimensional array comprehensions
- Lightweight "green" threading (coroutines)
- Call C functions directly (no wrappers or special APIs needed)
- A powerful but unobstrusive type system via optional type annotation
- Elegant and extensible automatic conversions for numeric and other types
- Lisp-like macros and other powerful metaprogramming facilities
- Efficient support for Unicode, including but not limited to UTF-8
- Powerful shell-like capabilities for managing other processes


Rationale
---------

Scientific computing has traditionally required the highest performance, yet domain experts have largely moved to slower dynamic languages for daily work. We believe there are many good reasons to prefer dynamic languages for these applications, and we don't expect their use to diminish any time soon. Fortunately, modern language design and compiler techniques make it possible to mostly eliminate the performance trade-off and provide a single environment productive enough for prototyping and performant enough for deploying applications. However, an open-source language with these characteristics has not emerged. Our project, Julia, fills this gap.

MATLAB generally dominates the field of programming languages for the applied sciences. We take note of some of the reasons for its success: it is well-suited to linear algebra and array manipulation, easy to use, and achieves impressive performance due to a JIT compiler (introduced in 2003). Several open source systems (Python/NumPy, R, Octave, SciLab) offer comparable advantages, but in particular have not kept up in the last category, and are still mostly not quite as naturally suited to linear algebra as MATLAB is.

Closing this performance gap is a primary goal of ours, but at the same time we feel there is room to increase power, flexibility, and usability of numerical languages for general programming. The rising importance of parallel computing and cloud computing provides further motivation to design a new system keeping those concerns in mind from the beginning.


Compiling
---------

Required build tools:

- gcc
- g++
- gfortran
- GNU make
- curl (to download external libraries)
  * fdlibm
  * OpenBLAS
  * LAPACK
  * ARPACK
  * pcre
  * readline
  * mongoose

Supported platforms:

- x86 and x86/64 GNU/Linux
- x86/64 OS X (x86 OS X should work but is untested)

To compile:

- Run `make` in `external/` to download and build external dependencies.
- Run `make` in the top-level directory to build julia.

No installation is required — julia is currently run from the directory where it was built. You might want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.


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
