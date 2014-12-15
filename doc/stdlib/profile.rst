.. module:: Base.Profile

.. _stdlib-profiling:

Profiling
=========

The ``Profile`` module provides tools to help developers improve the
performance of their code. When used, it takes measurements on running
code, and produces output that helps you understand how much time is
spent on individual line(s).  The most common usage is to identify
"bottlenecks" as targets for optimization.

``Profile`` implements what is known as a "sampling" or `statistical
profiler
<http://en.wikipedia.org/wiki/Profiling_(computer_programming)>`_.  It
works by periodically taking a backtrace during the execution of any
task. Each backtrace captures the currently-running function and
line number, plus the complete chain of function calls that led to
this line, and hence is a "snapshot" of the current state of
execution.

If much of your run time is spent executing a particular line of code,
this line will show up frequently in the set of all backtraces.  In
other words, the "cost" of a given line---or really, the cost of the
sequence of function calls up to and including this line---is
proportional to how often it appears in the set of all backtraces.

A sampling profiler does not provide complete line-by-line coverage,
because the backtraces occur at intervals (by default, 1 ms on Unix
systems and 10 ms on Windows, although the actual scheduling is
subject to operating system load). Moreover, as discussed further
below, because samples are collected at a sparse subset of all
execution points, the data collected by a sampling profiler is subject
to statistical noise.

Despite these limitations, sampling profilers have substantial strengths:

- You do not have to make any modifications to your code to take
  timing measurements (in contrast to the alternative `instrumenting
  profiler <https://github.com/timholy/IProfile.jl>`_).
- It can profile into Julia's core code and even (optionally) into C
  and Fortran libraries.
- By running "infrequently" there is very little performance overhead;
  while profiling, your code can run at nearly native speed.

For these reasons, it's recommended that you try using the built-in
sampling profiler before considering any alternatives.

Basic usage
-----------

Let's work with a simple test case::

    function myfunc()
        A = rand(100, 100, 200)
        maximum(A)
    end

It's a good idea to first run the code you intend to profile at least
once (unless you want to profile Julia's JIT-compiler)::

    julia> myfunc()  # run once to force compilation

Now we're ready to profile this function::

    julia> @profile myfunc()

To see the profiling results, there is a `graphical browser
<https://github.com/timholy/ProfileView.jl>`_ available, but here
we'll use the text-based display that comes with the standard library::

    julia> Profile.print()
          23 client.jl; _start; line: 373
            23 client.jl; run_repl; line: 166
               23 client.jl; eval_user_input; line: 91
                  23 profile.jl; anonymous; line: 14
                     8  none; myfunc; line: 2
                      8 dSFMT.jl; dsfmt_gv_fill_array_close_open!; line: 128
                     15 none; myfunc; line: 3
                      2  reduce.jl; max; line: 35
                      2  reduce.jl; max; line: 36
                      11 reduce.jl; max; line: 37

Each line of this display represents a particular spot (line number)
in the code.  Indentation is used to indicate the nested sequence of
function calls, with more-indented lines being deeper in the sequence
of calls.  In each line, the first "field" indicates the number of
backtraces (samples) taken *at this line or in any functions executed
by this line*. The second field is the file name, followed by a
semicolon; the third is the function name followed by a semicolon, and
the fourth is the line number.  Note that the specific line numbers
may change as Julia's code changes; if you want to follow along, it's
best to run this example yourself.

In this example, we can see that the top level is ``client.jl``'s
``_start`` function. This is the first Julia function that gets called
when you launch julia.  If you examine line 373 of ``client.jl``,
you'll see that (at the time of this writing) it calls ``run_repl``,
mentioned on the second line. This in turn calls ``eval_user_input``.
These are the functions in ``client.jl`` that interpret what you type
at the REPL, and since we're working interactively these functions
were invoked when we entered ``@profile myfunc()``.  The next line
reflects actions taken in the ``@profile`` macro.

The first line shows that 23 backtraces were taken at line 373 of
``client.jl``, but it's not that this line was "expensive" on its own:
the second line reveals that all 23 of these backtraces were actually
triggered inside its call to ``run_repl``, and so on. To find out
which operations are actually taking the time, we need to look deeper
in the call chain.

The first "important" line in this output is this one::

                     8  none; myfunc; line: 2

``none`` refers to the fact that we defined ``myfunc`` in the REPL,
rather than putting it in a file; if we had used a file, this would
show the file name. Line 2 of ``myfunc()`` contains the call to
``rand``, and there were 8 (out of 23) backtraces that occurred at
this line. Below that, you can see a call to
``dsfmt_gv_fill_array_close_open!`` inside ``dSFMT.jl``. You might be surprised not to see the
``rand`` function listed explicitly: that's because ``rand`` is *inlined*,
and hence doesn't appear in the backtraces.

A little further down, you see::

   15 none; myfunc; line: 3

Line 3 of ``myfunc`` contains the call to ``max``, and there were 15
(out of 23) backtraces taken here. Below that, you can see the
specific places in ``base/reduce.jl`` that carry out the
time-consuming operations in the ``max`` function for this type of
input data.

Overall, we can tentatively conclude that finding the maximum element
is approximately twice as expensive as generating the random
numbers. We could increase our confidence in this result by collecting
more samples::

    julia> @profile (for i = 1:100; myfunc(); end)
    
    julia> Profile.print()
           3121 client.jl; _start; line: 373
            3121 client.jl; run_repl; line: 166
               3121 client.jl; eval_user_input; line: 91
                  3121 profile.jl; anonymous; line: 1
                     848  none; myfunc; line: 2
                      842 dSFMT.jl; dsfmt_gv_fill_array_close_open!; line: 128
                     1510 none; myfunc; line: 3
                      74   reduce.jl; max; line: 35
                      122  reduce.jl; max; line: 36
                      1314 reduce.jl; max; line: 37

In general, if you have ``N`` samples collected at a line, you can
expect an uncertainty on the order of ``sqrt(N)`` (barring other
sources of noise, like how busy the computer is with other tasks). The
major exception to this rule is garbage-collection, which runs
infrequently but tends to be quite expensive. (Since julia's garbage
collector is written in C, such events can be detected using the
``C=true`` output mode described below, or by using `ProfileView
<https://github.com/timholy/ProfileView.jl>`_.)

This illustrates the default "tree" dump; an alternative is the "flat"
dump, which accumulates counts independent of their nesting::

    julia> Profile.print(format=:flat)
     Count File         Function                         Line
      3121 client.jl    _start                            373
      3121 client.jl    eval_user_input                    91
      3121 client.jl    run_repl                          166
       842 dSFMT.jl     dsfmt_gv_fill_array_close_open!   128
       848 none         myfunc                              2
      1510 none         myfunc                              3
      3121 profile.jl   anonymous                           1
        74 reduce.jl    max                                35
       122 reduce.jl    max                                36
      1314 reduce.jl    max                                37

If your code has recursion, one potentially-confusing point is that a
line in a "child" function can accumulate more counts than there are
total backtraces. Consider the following function definitions::

    dumbsum(n::Integer) = n == 1 ? 1 : 1 + dumbsum(n-1)
    dumbsum3() = dumbsum(3)

If you were to profile ``dumbsum3``, and a backtrace was taken while it was executing ``dumbsum(1)``, the backtrace would look like this::

    dumbsum3
        dumbsum(3)
            dumbsum(2)
                dumbsum(1)

Consequently, this child function gets 3 counts, even though the
parent only gets one.  The "tree" representation makes this much
clearer, and for this reason (among others) is probably the most
useful way to view the results.

Accumulation and clearing
-------------------------

Results from ``@profile`` accumulate in a buffer; if you run multiple
pieces of code under ``@profile``, then ``Profile.print()`` will show
you the combined results. This can be very useful, but sometimes you
want to start fresh; you can do so with ``Profile.clear()``.


Options for controlling the display of profile results
------------------------------------------------------

``Profile.print()`` has more options than we've described so far.
Let's see the full declaration::

    function print(io::IO = STDOUT, data = fetch(); format = :tree, C = false, combine = true, cols = tty_cols())

Let's discuss these arguments in order:

- The first argument allows you to save the results to a file, but the
  default is to print to ``STDOUT`` (the console).
- The second argument contains the data you want to analyze; by
  default that is obtained from ``Profile.fetch()``, which pulls out
  the backtraces from a pre-allocated buffer. For example, if you want
  to profile the profiler, you could say::

     data = copy(Profile.fetch())
     Profile.clear()
     @profile Profile.print(STDOUT, data) # Prints the previous results
     Profile.print()                      # Prints results from Profile.print()

- The first keyword argument, ``format``, was introduced above. The
  possible choices are ``:tree`` and ``:flat``.
- ``C``, if set to ``true``, allows you to see even the calls to C
  code.  Try running the introductory example with ``Profile.print(C =
  true)``. This can be extremely helpful in deciding whether it's
  Julia code or C code that is causing a bottleneck; setting
  ``C=true`` also improves the interpretability of the nesting, at
  the cost of longer profile dumps.
- Some lines of code contain multiple operations; for example, ``s +=
  A[i]`` contains both an array reference (``A[i]``) and a sum
  operation.  These correspond to different lines in the generated
  machine code, and hence there may be two or more different addresses
  captured during backtraces on this line.  ``combine=true`` lumps
  them together, and is probably what you typically want, but you can
  generate an output separately for each unique instruction pointer
  with ``combine=false``.
- ``cols`` allows you to control the number of columns that you are
  willing to use for display.  When the text would be wider than the
  display, you might see output like this::

                                33 inference.jl; abstract_call; line: 645
                                  33 inference.jl; abstract_call; line: 645
                                    33 ...rence.jl; abstract_call_gf; line: 567
                                       33 ...nce.jl; typeinf; line: 1201
                                     +1 5  ...nce.jl; ...t_interpret; line: 900
                                     +3 5 ...ence.jl; abstract_eval; line: 758
                                     +4 5 ...ence.jl; ...ct_eval_call; line: 733
                                     +6 5 ...ence.jl; abstract_call; line: 645

  File/function names are sometimes truncated (with ``...``),
  and indentation is truncated with a ``+n`` at the beginning,
  where ``n`` is the number of extra spaces that would have been
  inserted, had there been room. If you want a complete profile
  of deeply-nested code, often a good idea is to save to a file and
  use a very wide ``cols`` setting::

    s = open("/tmp/prof.txt","w")
    Profile.print(s,cols = 500)
    close(s)


Configuration
-------------

``@profile`` just accumulates backtraces, and the analysis happens
when you call ``Profile.print()``. For a long-running computation,
it's entirely possible that the pre-allocated buffer for storing
backtraces will be filled. If that happens, the backtraces stop but
your computation continues. As a consequence, you may miss some
important profiling data (you will get a warning when that happens).

You can obtain and configure the relevant parameters this way::

  Profile.init()            # returns the current settings
  Profile.init(n, delay)
  Profile.init(delay = 0.01)

``n`` is the total number of instruction pointers you can store, with
a default value of ``10^6``. If your typical backtrace is 20
instruction pointers, then you can collect 50000 backtraces, which
suggests a statistical uncertainty of less than 1%. This may be good
enough for most applications.

Consequently, you are more likely to need to modify ``delay``,
expressed in seconds, which sets the amount of time that Julia gets
between snapshots to perform the requested computations. A very
long-running job might not need frequent backtraces. The default
setting is ``delay = 0.001``.  Of course, you can decrease the delay
as well as increase it; however, the overhead of profiling grows once
the delay becomes similar to the amount of time needed to take a
backtrace (~30 microseconds on the author's laptop).

.. _stdlib-track-allocation:

Direct analysis of memory allocation
====================================

One of the most common techniques to improve performance is to reduce
memory allocation.  The total amount of allocation can be measured
with ``@time`` and ``@allocated``, and specific lines triggering
allocation can often be inferred from profiling via the cost of
garbage collection that these lines incur.  However, sometimes it is
more efficient to directly measure the amount of memory allocated by
each line of code.

To measure allocation line-by-line, start julia with the
``--track-allocation=<setting>`` command-line option, for which you
can choose ``none`` (the default, do not measure allocation), ``user``
(measure memory allocation everywhere except julia's core code), or
``all`` (measure memory allocation at each line of julia code).
Allocation gets measured for each line of compiled code.  When you quit
julia, the cumulative results are written to text files with ``.mem``
appended after the file name, residing in the same directory as the
source file.  Each line lists the total number of bytes allocated.
The ``Coverage`` package contains some elementary analysis tools, for
example to sort the lines in order of number of bytes allocated.

In interpreting the results, there are a few important details.  Under
the ``user`` setting, the first line of any function directly called
from the REPL will exhibit allocation due to events that happen in the
REPL code itself.  More significantly, JIT-compilation also adds to
allocation counts, because much of julia's compiler is written in
Julia (and compilation usually requires memory allocation).  The
recommended procedure it to force compilation by executing all the
commands you want to analyze, then call ``clear_malloc_data()`` to
reset all allocation counters.  Finally, execute the desired commands
and quit julia to trigger the generation of the ``.mem`` files.

Function reference
------------------

.. currentmodule:: Base

.. function:: @profile

   ``@profile <expression>`` runs your expression while taking
   periodic backtraces.  These are appended to an internal buffer of
   backtraces.

.. currentmodule:: Base.Profile

.. function:: clear()

   Clear any existing backtraces from the internal buffer.

.. function:: print([io::IO = STDOUT,] [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   Prints profiling results to ``io`` (by default, ``STDOUT``). If you
   do not supply a ``data`` vector, the internal buffer of accumulated
   backtraces will be used.  ``format`` can be ``:tree`` or
   ``:flat``. If ``C==true``, backtraces from C and Fortran code are
   shown. ``combine==true`` merges instruction pointers that
   correspond to the same line of code.  ``cols`` controls the width
   of the display.

.. function:: print([io::IO = STDOUT,] data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to ``io``. This variant is used to examine
   results exported by a previous call to ``Profile.retrieve()``.
   Supply the vector ``data`` of backtraces and a dictionary
   ``lidict`` of line information.

.. function:: init(; n::Integer, delay::Float64)

   Configure the ``delay`` between backtraces (measured in seconds),
   and the number ``n`` of instruction pointers that may be
   stored. Each instruction pointer corresponds to a single line of
   code; backtraces generally consist of a long list of instruction
   pointers. Default settings can be obtained by calling this function
   with no arguments, and each can be set independently using keywords
   or in the order ``(n, delay)``.

.. function:: fetch() -> data

   Returns a reference to the internal buffer of backtraces. Note that
   subsequent operations, like ``Profile.clear()``, can affect
   ``data`` unless you first make a copy. Note that the values in
   ``data`` have meaning only on this machine in the current session,
   because it depends on the exact memory addresses used in
   JIT-compiling. This function is primarily for internal use;
   ``Profile.retrieve()`` may be a better choice for most users.

.. function:: retrieve() -> data, lidict

   "Exports" profiling results in a portable format, returning the set
   of all backtraces (``data``) and a dictionary that maps the
   (session-specific) instruction pointers in ``data`` to ``LineInfo``
   values that store the file name, function name, and line
   number. This function allows you to save profiling results for
   future analysis.

.. function::callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>]) -> Vector{(count, linfo)}

   Given a previous profiling run, determine who called a particular
   function. Supplying the filename (and optionally, range of line
   numbers over which the function is defined) allows you to
   disambiguate an overloaded method. The returned value is a vector
   containing a count of the number of calls and line information
   about the caller.  One can optionally supply backtrace data
   obtained from ``retrieve``; otherwise, the current internal profile
   buffer is used.

.. function:: clear_malloc_data()

   Clears any stored memory allocation data when running julia with
   ``--track-allocation``.  Execute the command(s) you want to test
   (to force JIT-compilation), then call ``clear_malloc_data()``.
   Then execute your command(s) again, quit julia, and examine the
   resulting ``*.mem`` files.
