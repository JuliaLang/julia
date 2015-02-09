*************************
Using Valgrind with Julia
*************************

`Valgrind <http://valgrind.org/>`_ is a tool for memory debugging, memory leak detection, and profiling.  This section describes things to keep in mind when using Valgrind to debug memory issues with Julia.

Suppressions
------------

Valgrind will typically display spurious warnings as it runs.  To reduce the number of such warnings, it helps to provide a `suppressions file <http://valgrind.org/docs/manual/manual-core.html#manual-core.suppress>`_ to Valgrind.  A sample suppressions file is included in the Julia source distribution at ``contrib/valgrind-julia.supp``.

The suppressions file can be used from the ``julia/`` source directory as follows::

    $ valgrind --suppressions=contrib/valgrind-julia.supp ./julia progname.jl

Any memory errors that are displayed should either be reported as bugs or contributed as additional suppressions.  Note that some versions of Valgrind are `shipped with insufficient default suppressions <https://github.com/JuliaLang/julia/issues/8314#issuecomment-55766210>`_, so that may be one thing to consider before submitting any bugs.
