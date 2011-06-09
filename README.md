                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |

Julia is a very high level dynamic language with optional typing, multiple dispatch, and good performance, achieved by using type inference and just-in-time (JIT) compilation, implemented using LLVM. The language is multi-paradigm, combining features of functional, object-oriented, and imperative traditions.

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

For a more in depth discussion of the rationale and advantages of Julia over other systems, see the [Introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the wiki, or [browse](https://github.com/JuliaLang/julia/wiki/) all of the wiki documentation.

## Resources

- **Homepage:** <http://julialang.org>
- **Discussion:** <julia-math@googlegroups.com>
- **Download:** <https://github.com/JuliaLang/julia>

## Required Build Tools

- gcc
- g++
- gfortran
- GNU make
- curl to download external libraries:
  * fdlibm
  * OpenBLAS
  * LAPACK
  * ARPACK
  * pcre
  * readline
  * mongoose

## Supported Platforms

- x86 and x86/64 GNU/Linux
- x86/64 OS X (x86 OS X should work but is untested)

## Compilation

- Run `make` in `external/` to download and build external dependencies.
- Run `make` in the top-level directory to build julia.

No installation is required — julia is currently run from the directory where it was built. You might want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.

## Directories

    attic/         old, now-unused code
    bench/         benchmarks and performance tests
    contrib/       emacs and textmate support for julia
    doc/           TeX manual and other notes
    external/      external dependencies
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function tests for julia itself
    ui/            source for various frontends

## Emacs Setup

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.
