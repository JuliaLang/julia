The Julia Debugger
------------------

In order to have a Julia debugger, we first need a version of LLDB
that understands Julia's JITed stack frames. To do so, build Julia
with instructions from https://github.com/Keno/Cxx.jl. Just the julia
parts are required. The Cxx part won't be necessary until the debugger
UI is released.

Use LLDB just as you would usually.

This should at least get stack traces and the ability to step
through julia code. Please file any issues as bug reports.

For local variables, you may get some, and you'll get more if you set
`DISABLE_OPT` in `src/options.h`. If you set the latter, it is useful
to collect a set of test cases that don't get local variables even
though they should. You will probably find a lot of functions don't
work, yet.

OS X
====

* Enable FORCE_ELF in src/options.

* Comment out `if (arch.GetTriple().getVendor() != llvm::Triple::Apple)`
in deps/llvm-svn/tools/lldb/source/Plugins/JITLoader/GDB/JITLoaderGDB.cpp

* export `LLDB_DEBUGSERVER_PATH=/Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Resources/debugserver`

References
==========

The original post on julia-dev: https://groups.google.com/forum/#!topic/julia-dev/gcZ5dZJni5o
