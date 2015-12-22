.. _man-stacktraces:

.. currentmodule:: Base

Stack Traces
============

The :mod:`StackTraces` module provides simple stack traces that are both human readable and easy to use programmatically.

Viewing a stack trace
---------------------

The primary function used to obtain a stack trace is :func:`stacktrace`::

    julia> using StackTraces

    julia> stacktrace()
    2-element Array{StackTraces.StackFrame,1}:
     StackTraces.StackFrame(:eval_user_input,symbol("REPL.jl"),62,symbol(""),-1,false,13041465684)
     StackTraces.StackFrame(:anonymous,symbol("REPL.jl"),92,symbol("task.jl"),63,false,1304140086)

Calling :func:`stacktrace` returns a vector of :obj:`StackFrame` s. For ease of use, the alias :obj:`StackTrace` can be used in place of ``Vector{StackFrame}``.

::

    julia> example() = stacktrace()
    example (generic function with 1 method)

    julia> example()
    3-element Array{StackTraces.StackFrame,1}:
     StackTraces.StackFrame(:example,:none,1,symbol(""),-1,false,13041535346)
     StackTraces.StackFrame(:eval_user_input,symbol("REPL.jl"),62,symbol(""),-1,false,13041465684)
     StackTraces.StackFrame(:anonymous,symbol("REPL.jl"),92,symbol("task.jl"),63,false,13041400866)

If you'd like the output to be a little more human-readable, replace calls to :func:`stacktrace` (which returns a vector of :obj:`StackFrame` s) with :func:`show_stacktrace` (which prints the stacktrace to an IO stream).

::

    julia> example() = show_stacktrace()
    example (generic function with 1 method)

    julia> example()
    StackTrace with 3 StackFrames:
      example at none:1
      eval_user_input at REPL.jl:62
      [inlined code from REPL.jl:92] anonymous at task.jl:63

Note that when calling :func:`stacktrace` from the REPL you'll always have those last two frames in the stack from ``REPL.jl`` (including the anonymous function from ``task.jl``).

::

    julia> @noinline child() = show_stacktrace()
    child (generic function with 1 method)

    julia> @noinline parent() = child()
    parent (generic function with 1 method)

    julia> grandparent() = parent()
    grandparent (generic function with 1 method)

    julia> grandparent()
    StackTrace with 5 StackFrames:
      child at none:1
      parent at none:1
      grandparent at none:1
      eval_user_input at REPL.jl:62
      [inlined code from REPL.jl:92] anonymous at task.jl:63

Extracting useful information
-----------------------------

Each :obj:`StackFrame` contains the function name, file name, line number, file and line information for inlined functions, a flag indicating whether it is a C function (by default C functions do not appear in the stack trace), and an integer representation of the pointer returned by :func:`backtrace`::

    julia> top_frame = stacktrace()[1]
    StackTraces.StackFrame(:eval_user_input,symbol("REPL.jl"),62,symbol(""),-1,false, 13203085684)

    julia> top_frame.func
    :eval_user_input

    julia> top_frame.file
    symbol("REPL.jl")

    julia> top_frame.line
    62

    julia> top_frame.inlined_file
    symbol("")

    julia> top_frame.inlined_line
    -1

    julia> top_frame.from_c
    false

    julia> top_frame.pointer
    13203085684

This makes stack trace information available programmatically without having to capture and parse the output from something like ``Base.show_backtrace(io, backtrace())``.

Error handling
--------------

While having easy access to information about the current state of the callstack can be helpful in many places, the most obvious application is in error handling and debugging.

::

    julia> example() = try
               error("Oh no!")
           catch
               show_stacktrace()
           end
    example (generic function with 1 method)

    julia> example()
    StackTrace with 3 StackFrames:
      example at none:4
      eval_user_input at REPL.jl:62
      [inlined code from REPL.jl:92] anonymous at task.jl:63

You may notice that in the example above the first stack frame points points at line 4, where :func:`stacktrace` is called, rather than line 2, where the error occurred. While in this example it's trivial to track down the actual source of the error, things can get misleading pretty quickly if the stack trace doesn't even point to the right function.

This can be remedied by calling :func:`catch_stacktrace` instead of :func:`stacktrace`. Instead of returning callstack information for the current context, :func:`catch_stacktrace` returns stack information for the context of the most recent error::

    julia> example() = try
               error("Oh no!")
           catch
               show_stacktrace(catch_stacktrace())
           end
    example (generic function with 1 method)

    julia> example()
    StackTrace with 3 StackFrames:
      example at none:2
      eval_user_input at REPL.jl:62
      [inlined code from REPL.jl:92] anonymous at task.jl:63

Notice that the stack trace now indicates the appropriate line number.

::

    julia> @noinline child() = error("Whoops!")
    child (generic function with 1 method)

    julia> @noinline parent() = child()
    parent (generic function with 1 method)

    julia> function grandparent()
               try
                   parent()
               catch err
                   println("ERROR: ", err.msg)
                   show_stacktrace(catch_stacktrace())
               end
           end
    grandparent (generic function with 1 method)

    julia> grandparent()
    ERROR: Whoops!
    StackTrace with 5 StackFrames:
      child at none:1
      parent at none:1
      grandparent at none:3
      eval_user_input at REPL.jl:62
      [inlined code from REPL.jl:92] anonymous at task.jl:63

Comparison with ``backtrace``
-----------------------------

A call to :func:`backtrace` returns a vector of ``Ptr{Void}``, which may then be passed into :func:`stacktrace` for translation::

    julia> stack = backtrace()
    15-element Array{Ptr{Void},1}:
     Ptr{Void} @0x000000010e9562ed
     Ptr{Void} @0x0000000312f95f20
     Ptr{Void} @0x0000000312f95ea0
     Ptr{Void} @0x000000010e8e5776
     Ptr{Void} @0x000000010e950c04
     Ptr{Void} @0x000000010e94f2a8
     Ptr{Void} @0x000000010e94f137
     Ptr{Void} @0x000000010e95070d
     Ptr{Void} @0x000000010e95053f
     Ptr{Void} @0x000000010e963348
     Ptr{Void} @0x000000010e8edd67
     Ptr{Void} @0x0000000312f71974
     Ptr{Void} @0x0000000312f715c7
     Ptr{Void} @0x0000000312f65c22
     Ptr{Void} @0x000000010e95708f

    julia> stacktrace(stack)
    3-element Array{StackTraces.StackFrame,1}:
     StackTraces.StackFrame(:backtrace,symbol("error.jl"),26,symbol(""),-1,false,13203234592)
     StackTraces.StackFrame(:eval_user_input,symbol("REPL.jl"),62,symbol(""),-1,false,13203085684)
     StackTraces.StackFrame(:anonymous,symbol("REPL.jl"),92,symbol("task.jl"),63,false,13203037218)

Notice that the vector returned by :func:`backtrace` had 15 pointers, while the vector returned by :func:`stacktrace` only has 3. This is because, by default, :func:`stacktrace` removes any lower-level C functions from the stack. If you want to include stack frames from C calls, you can do it like this::

    julia> stacktrace(stack, true)
    15-element Array{StackTraces.StackFrame,1}:
     StackTraces.StackFrame(:rec_backtrace,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/task.c"),644,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/task.c"),703,true,4539638509)
     StackTraces.StackFrame(:backtrace,symbol("error.jl"),26,symbol(""),-1,false,13203234592)
     StackTraces.StackFrame(:jlcall_backtrace_21483,symbol(""),-1,symbol(""),-1,true,13203234464)
     StackTraces.StackFrame(:jl_apply,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/gf.c"),1691,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/gf.c"),1708,true,4539176822)
     StackTraces.StackFrame(:jl_apply,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),55,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),65,true,4539616260
     StackTraces.StackFrame(:eval,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),213,symbol(""),-1,true,4539609768)
     StackTraces.StackFrame(:eval,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),219,symbol(""),-1,true,4539609399)
     StackTraces.StackFrame(:eval_body,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),592,symbol(""),-1,true,4539614989)
     StackTraces.StackFrame(:jl_toplevel_eval_body,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/interpreter.c"),527,symbol(""),-1,true,4539614527)
     StackTraces.StackFrame(:jl_toplevel_eval_flex,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/toplevel.c"),521,symbol(""),-1,true,4539691848)
     StackTraces.StackFrame(:jl_toplevel_eval_in,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/builtins.c"),579,symbol(""),-1,true,4539211111)
     StackTraces.StackFrame(:eval_user_input,symbol("REPL.jl"),62,symbol(""),-1,false,13203085684)
     StackTraces.StackFrame(:jlcall_eval_user_input_21232,symbol(""),-1,symbol(""),-1,true,13203084743)
     StackTraces.StackFrame(:anonymous,symbol("REPL.jl"),92,symbol("task.jl"),63,false,13203037218)
     StackTraces.StackFrame(:jl_apply,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/task.c"),241,symbol("/private/tmp/julia20151107-44794-o1d6wy/src/task.c"),240,true,4539641999)
