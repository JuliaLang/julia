.. _man-getting-started:

*****************
 Getting Started  
*****************

Julia installation is straightforward, whether using precompiled
binaries or compiling from source. Download and install Julia by
following the instructions at
`http://julialang.org/downloads/ <http://julialang.org/downloads/>`_.

The easiest way to learn and experiment with Julia is by starting an
interactive session (also known as a read-eval-print loop or "repl")::

    $ julia
                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |  A fresh approach to technical computing.
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |  Version 0 (pre-release)
      | | |_| | | | (_| |  |  Commit 61847c5aa7 (2011-08-20 06:11:31)*
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |

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
``--machinefile`` options. ``-p n`` will launch an additional ``n`` 
worker processes, while ``--machinefile file`` will launch a worker 
for each line in file ``file``. The machines defined in ``file`` must be 
accessible via ``ssh`` and each machine definition takes the form 
``[user@]host[:port]``
    
    
If you have code that you want executed whenever julia is run, you can
put it in ``~\.juliarc.jl``::

    $ echo 'println("Greetings! 你好! 안녕하세요?")' > ~/.juliarc.jl
    $ julia
    Greetings! 你好! 안녕하세요?
    
    ...

There are various ways to run Julia code and provide options, similar to
those available for the ``perl`` and ``ruby`` programs::

    julia [options] [program] [args...]
     -v --version             Display version information
     -q --quiet               Quiet startup without banner
     -H --home=<dir>          Load files relative to <dir>
     -T --tab=<size>          Set REPL tab width to <size>

     -e --eval=<expr>         Evaluate <expr>
     -E --print=<expr>        Evaluate and show <expr>
     -P --post-boot=<expr>    Evaluate <expr> right after boot
     -L --load=file           Load <file> right after boot on all processors
     -J --sysimage=file       Start up with the given system image file

     -p n                     Run n local processes
     --machinefile file       Run processes on hosts listed in file

     --no-history             Don't load or save history
     -f --no-startup          Don't load ~/.juliarc.jl
     -F                       Load ~/.juliarc.jl, then handle remaining inputs
     --color=yes|no           Enable or disable color text

     -h --help                Print this message

Resources
---------

In addition to this manual, there are various other resources that may
help new users get started with julia:

- `Julia and IJulia cheatsheet <http://math.mit.edu/%7Estevenj/Julia-cheatsheet.pdf>`_
- `Learn Julia in a few minutes <http://learnxinyminutes.com/docs/julia/>`_
- `Tutorial for Homer Reid's numerical analysis class <http://homerreid.ath.cx/teaching/18.330/JuliaProgramming.shtml#SimplePrograms>`_
- `An introductory presentation <https://github.com/ViralBShah/julia-presentations/raw/master/Fifth-Elephant-2013/Fifth-Elephant-2013.pdf>`_
- `Videos from the Julia tutorial at MIT <http://julialang.org/blog/2013/03/julia-tutorial-MIT/>`_
- `Forio Julia Tutorials <http://forio.com/julia/tutorials-list>`_

