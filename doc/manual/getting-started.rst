.. _man-getting-started:

*****************
 Getting Started  
*****************

Julia installation is straightforward, whether using precompiled
binaries or compiling from source. Download and install Julia by
following the instructions at
`http://julialang.org/downloads/ <http://julialang.org/downloads/>`_.

The easiest way to learn and experiment with Julia is by starting an
interactive session (also known as a read-eval-print loop or "repl")
by double-clicking the Julia executable or running ``julia`` from the
command line::

    $ julia
                   _
       _       _ _(_)_     |  A fresh approach to technical computing
      (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
       _ _   _| |_  __ _   |  Type "help()" to list help topics
      | | | | | | |/ _` |  |
      | | |_| | | | (_| |  |  Version 0.3.0-prerelease+3690 (2014-06-16 05:11 UTC)
     _/ |\__'_|_|_|\__'_|  |  Commit 1b73f04* (0 days old master)
    |__/                   |  x86_64-apple-darwin13.1.0

    julia> 1 + 2
    3

    julia> ans
    3

To exit the interactive session, type ``^D`` — the control key
together with the ``d`` key or type ``quit()``. When run in interactive
mode, ``julia`` displays a banner and prompts the user for input. Once
the user has entered a complete expression, such as ``1 + 2``, and
hits enter, the interactive session evaluates the expression and shows
its value. If an expression is entered into an interactive session
with a trailing semicolon, its value is not shown. The variable
``ans`` is bound to the value of the last evaluated expression whether
it is shown or not. The ``ans`` variable is only bound in interactive
sessions, not when Julia code is run in other ways.

To evaluate expressions written in a source file ``file.jl``, write
``include("file.jl")``.

To run code in a file non-interactively, you can give it as the first
argument to the julia command::

    $ julia script.jl arg1 arg2...

As the example implies, the following command-line arguments to julia
are taken as command-line arguments to the program ``script.jl``, passed
in the global constant ``ARGS``. ``ARGS`` is also set when script code
is given using the ``-e`` option on the command line (see the ``julia``
help output below). For example, to just print the arguments given to a
script, you could do this::

    $ julia -e 'for x in ARGS; println(x); end' foo bar
    foo
    bar

Or you could put that code into a script and run it::

    $ echo 'for x in ARGS; println(x); end' > script.jl
    $ julia script.jl foo bar
    foo
    bar

Julia can be started in parallel mode with either the ``-p`` or the
``--machinefile`` options. ``-p n`` will launch an additional ``n`` worker
processes, while ``--machinefile file`` will launch a worker for each line in
file ``file``. The machines defined in ``file`` must be accessible via a
passwordless ``ssh`` login, with Julia installed at the same location as the
current host. Each machine definition takes the form
``[count*][user@]host[:port] [bind_addr[:port]]`` . ``user`` defaults to current user,
``port`` to the standard ssh port. ``count`` is the number of workers to spawn
on the node, and defaults to 1. The optional ``bind-to bind_addr[:port]`` 
specifies the ip-address and port that other workers should use to 
connect to this worker.
    
    
If you have code that you want executed whenever julia is run, you can
put it in ``~/.juliarc.jl``:

.. raw:: latex

    \begin{CJK*}{UTF8}{mj}

::

    $ echo 'println("Greetings! 你好! 안녕하세요?")' > ~/.juliarc.jl
    $ julia
    Greetings! 你好! 안녕하세요?
    
    ...

.. raw:: latex

    \end{CJK*}

There are various ways to run Julia code and provide options, similar to
those available for the ``perl`` and ``ruby`` programs::

    julia [options] [program] [args...]
     -v, --version            Display version information
     -h, --help               Print this message
     -q, --quiet              Quiet startup without banner
     -H, --home <dir>         Set location of julia executable

     -e, --eval <expr>        Evaluate <expr>
     -E, --print <expr>       Evaluate and show <expr>
     -P, --post-boot <expr>   Evaluate <expr>, but don't disable interactive mode
     -L, --load <file>        Load <file> immediately on all processors
     -J, --sysimage <file>    Start up with the given system image file

     -p <n>                   Run n local processes
     --machinefile <file>     Run processes on hosts listed in <file>
               
     -i                       Force isinteractive() to be true
     --no-history-file        Don't load or save history
     -f, --no-startup         Don't load ~/.juliarc.jl
     -F                       Load ~/.juliarc.jl, then handle remaining inputs
     --color={yes|no}         Enable or disable color text

     --compile={yes|no|all}   Enable or disable compiler, or request exhaustive compilation
                     
     --code-coverage={none|user|all}, --code-coverage
                              Count executions of source lines (omitting setting is equivalent to 'user')
     --track-allocation={none|user|all}
                              Count bytes allocated by each source line
     --check-bounds={yes|no}  Emit bounds checks always or never (ignoring declarations)
     -O, --optimize           Run time-intensive code optimizations
     --int-literals={32|64}   Select integer literal size independent of platform
     --dump-bitcode={yes|no}  Dump bitcode for the system image (used with --build)
     --depwarn={yes|no}       Enable or disable syntax and method deprecation warnings


Resources
---------

In addition to this manual, there are various other resources that may
help new users get started with julia:

- `Julia and IJulia cheatsheet <http://math.mit.edu/~stevenj/Julia-cheatsheet.pdf>`_
- `Learn Julia in a few minutes <http://learnxinyminutes.com/docs/julia/>`_
- `Tutorial for Homer Reid's numerical analysis class <http://homerreid.dyndns.org/teaching/18.330/JuliaProgramming.shtml>`_
- `An introductory presentation <https://raw.githubusercontent.com/ViralBShah/julia-presentations/master/Fifth-Elephant-2013/Fifth-Elephant-2013.pdf>`_
- `Videos from the Julia tutorial at MIT <http://julialang.org/blog/2013/03/julia-tutorial-MIT/>`_
- `Forio Julia Tutorials <http://forio.com/labs/julia-studio/tutorials/>`_

