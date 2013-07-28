Profiling
=========

.. module:: Base.Profile

The ``Profile`` module provides tools to help developers improve the
performance of their code. When used, it takes measurements on running
code, and produces output that helps you understand how much time is
spent on individual line(s).  The most common usage is to identify
"bottlenecks" as targets for optimization.

``Profile`` implements what is known as a "sampling" or `statistical
profiler
<http://en.wikipedia.org/wiki/Profiling_(computer_programming)>`_.  It
works by periodically taking a backtrace during the execution of any
task. Each backtrace captures the the currently-running function and
line number, plus the complete chain of function calls that led to
this line, and hence is a "snapshot" of the current state of
execution.  If you find that a particular line appears frequently in
the set of backtraces, you might suspect that much of the run-time is
spent on this line (and therefore is a bottleneck in your code).

These snapshots do not provide complete line-by-line coverage, because
they occur at intervals (by default, 1 ms). However, this design has
important strengths:

- You do not have to make any modifications to your code to take
  timing measurements (in contrast to the alternative `instrumenting
  profiler <https://github.com/timholy/Profile.jl>`_)
- It can profile into Julia's core code and even (optionally) into C
  and Fortran libraries
- By running "infrequently" there is very little performance overhead;
  while profiling, your code will run at nearly native speed.

Basic usage
-----------

Let's work with a simple test case::

    function myfunc()
        A = rand(100, 100, 200)
        max(A)
    end

It's a good idea to first run the code you intend to profile at least
once (unless you want to profile Julia's JIT-compiler)::

    julia> myfunc()  # run once to force compilation

Now we're ready to profile this function::

    julia> @profile myfunc()

Now let's see the results::

    julia> Profile.print()
          23 client.jl; _start; line: 373
            23 client.jl; run_repl; line: 166
               23 client.jl; eval_user_input; line: 91
                  23 profile.jl; anonymous; line: 14
                     8  none; myfunc; line: 2
                      8 librandom.jl; dsfmt_gv_fill_array_close_open!; line: 128
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
at the REPL, which is how we launched ``@profile``.  The next line
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
``dsfmt_gv_fill_array_close_open!`` inside ``librandom.jl``. You might be surprised not to see the
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
                      842 librandom.jl; dsfmt_gv_fill_array_close_open!; line: 128
                     1510 none; myfunc; line: 3
                      74   reduce.jl; max; line: 35
                      122  reduce.jl; max; line: 36
                      1314 reduce.jl; max; line: 37

In general, if you have ``N`` samples collected at a line, you can
expect an uncertainty on the order of ``sqrt(N)`` (barring other
sources of noise, like how busy the computer is with other tasks).

This illustrates the default "tree" dump; an alternative is the "flat"
dump, which accumulates counts independent of their nesting::

    julia> Profile.print(format=:flat)
     Count File         Function                         Line
      3121 client.jl    _start                            373
      3121 client.jl    eval_user_input                    91
      3121 client.jl    run_repl                          166
       842 librandom.jl dsfmt_gv_fill_array_close_open!   128
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

- The first named argument, ``format``, was introduced above. The
  possible choices are ``:tree`` and ``:flat``.
- ``C``, if set to ``true``, allows you to even see the calls to C
  code.  Try running the introductory example with ``Profile.print(C =
  true)``. This can be extremely helpful in deciding whether it's
  Julia code or C code that is causing a bottleneck; setting
  ``C=true`` also improves the interpretability of the nesting, at
  some cost in length.
- Some lines of code contain multiple operations; for example, ``s +=
  A[i]`` contains both an array reference (``A[i]``) and a sum
  operation.  These correspond to different lines in the generated
  machine code, and hence there may be two or more different addresses
  captured during backtraces on this line.  ``combine=true`` lumps
  them together, and is probably what you usually want, but you can
  generate an output separately for each unique instruction pointer
  with ``combine=false``.
- ``cols`` allows you to control the number of columns that you are
  willing to use for display.  When the text would be wider than the
  display, file/function names are sometimes truncated (with ``...``),
  and indentation is truncated (denoted by a ``+n`` at the beginning,
  where ``n`` is the number of extra spaces that would have been
  inserted, had there been room). If you want a very complete profile
  in deeply-nested code, often a good idea is to save to a file and
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

You can configure the relevant parameters this way::

  Profile.init(n, delay)

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
