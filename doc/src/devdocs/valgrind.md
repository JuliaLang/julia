# Using Valgrind with Julia

[Valgrind](https://valgrind.org/) is a tool for memory debugging, memory leak detection, and profiling.
 This section describes things to keep in mind when using Valgrind to debug memory issues with
Julia.

## General considerations

By default, Valgrind assumes that there is no self modifying code in the programs it runs.  This
assumption works fine in most instances but fails miserably for a just-in-time compiler like
`julia`.  For this reason it is crucial to pass `--smc-check=all-non-file` to `valgrind`, else
code may crash or behave unexpectedly (often in subtle ways).

In some cases, to better detect memory errors using Valgrind it can help to compile `julia` with
memory pools disabled.  The compile-time flag `MEMDEBUG` disables memory pools in Julia, and
`MEMDEBUG2` disables memory pools in FemtoLisp.  To build `julia` with both flags, add the following
line to `Make.user`:

```make
CFLAGS = -DMEMDEBUG -DMEMDEBUG2
```

Another thing to note: if your program uses multiple workers processes, it is likely that you
want all such worker processes to run under Valgrind, not just the parent process.  To do this,
pass `--trace-children=yes` to `valgrind`.

Yet another thing to note: if using `valgrind` errors with `Unable to find compatible target in system image`,
try rebuilding the sysimage with target `generic` or julia with `JULIA_CPU_TARGET=generic`.

## Suppressions

Valgrind will typically display spurious warnings as it runs.  To reduce the number of such warnings,
it helps to provide a [suppressions file](https://valgrind.org/docs/manual/manual-core.html#manual-core.suppress)
to Valgrind.  A sample suppressions file is included in the Julia source distribution at `contrib/valgrind-julia.supp`.

The suppressions file can be used from the `julia/` source directory as follows:

```
$ valgrind --smc-check=all-non-file --suppressions=contrib/valgrind-julia.supp ./julia progname.jl
```

Any memory errors that are displayed should either be reported as bugs or contributed as additional
suppressions.  Note that some versions of Valgrind are [shipped with insufficient default suppressions](https://github.com/JuliaLang/julia/issues/8314#issuecomment-55766210),
so that may be one thing to consider before submitting any bugs.

## Running the Julia test suite under Valgrind

It is possible to run the entire Julia test suite under Valgrind, but it does take quite some
time (typically several hours).  To do so, run the following command from the `julia/test/` directory:

```
valgrind --smc-check=all-non-file --trace-children=yes --suppressions=$PWD/../contrib/valgrind-julia.supp ../julia runtests.jl all
```

If you would like to see a report of "definite" memory leaks, pass the flags `--leak-check=full --show-leak-kinds=definite`
to `valgrind` as well.

## Additional spurious warnings

This section covers Valgrind warnings which cannot be added to the
suppressions file yet are nonetheless safe to ignore.

### Unhandled rr system calls

Valgrind will emit a warning if it encounters any of the [system calls
that are specific to
rr](https://github.com/rr-debugger/rr/blob/master/src/preload/rrcalls.h),
the [Record and Replay Framework](https://rr-project.org/).  In
particular, a warning about an unhandled `1008` syscall will be shown
when julia tries to detect whether it is running under rr:

```
--xxxxxx-- WARNING: unhandled amd64-linux syscall: 1008
--xxxxxx-- You may be able to write your own handler.
--xxxxxx-- Read the file README_MISSING_SYSCALL_OR_IOCTL.
--xxxxxx-- Nevertheless we consider this a bug.  Please report
--xxxxxx-- it at http://valgrind.org/support/bug_reports.html.
```

This issue
[has been reported](https://bugs.kde.org/show_bug.cgi?id=446401)
to the Valgrind developers as they have requested.

## Caveats

Valgrind currently [does not support multiple rounding modes](https://bugs.kde.org/show_bug.cgi?id=136779),
so code that adjusts the rounding mode will behave differently when run under Valgrind.

In general, if after setting `--smc-check=all-non-file` you find that your program behaves differently
when run under Valgrind, it may help to pass `--tool=none` to `valgrind` as you investigate further.
 This will enable the minimal Valgrind machinery but will also run much faster than when the full
memory checker is enabled.
