.. _man-stacktraces:

.. currentmodule:: Base

************
Stack Traces
************

The :mod:`StackTraces` module provides simple stack traces that are both human readable and
easy to use programmatically.

Viewing a stack trace
---------------------

The primary function used to obtain a stack trace is :func:`stacktrace`::

    julia> stacktrace()
    3-element Array{StackFrame,1}:
     eval at boot.jl:265
     [inlined code from REPL.jl:3] eval_user_input at REPL.jl:62
     [inlined code from REPL.jl:92] anonymous at task.jl:63

Calling :func:`stacktrace` returns a vector of :obj:`StackFrame` s. For ease of use, the
alias :obj:`StackTrace` can be used in place of ``Vector{StackFrame}``. (Examples with
``...`` indicate that output may vary depending on how the code is run.)

.. doctest::

    julia> example() = stacktrace()
    example (generic function with 1 method)

    julia> example()
    ...-element Array{StackFrame,1}:
      in example() at none:1
      in eval(::Module, ::Any) at boot.jl:237
      [inlined code] from sysimg.jl:11
     ...

    julia> @noinline child() = stacktrace()
    child (generic function with 1 method)

    julia> @noinline parent() = child()
    parent (generic function with 1 method)

    julia> grandparent() = parent()
    grandparent (generic function with 1 method)

    julia> grandparent()
    ...-element Array{StackFrame,1}:
      [inlined code] from stacktraces.jl:135
     in child() at none:1
      in parent() at none:1
      in grandparent() at none:1
      in eval(::Module, ::Any) at boot.jl:237
     ...

Note that when calling :func:`stacktrace` you'll typically see a frame with
``eval at boot.jl``. When calling :func:`stacktrace` from the REPL you'll also have a few
extra frames in the stack from ``REPL.jl``, usually looking something like this::

    julia> example() = stacktrace()
    example (generic function with 1 method)

    julia> example()
    4-element Array{StackFrame,1}:
     example at none:1
     eval at boot.jl:265
     [inlined code from REPL.jl:3] eval_user_input at REPL.jl:62
     [inlined code from REPL.jl:92] anonymous at task.jl:63

Extracting useful information
-----------------------------

Each :obj:`StackFrame` contains the function name, file name, line number, file and line
information for inlined functions, a flag indicating whether it is a C function (by default
C functions do not appear in the stack trace), and an integer representation of the pointer
returned by :func:`backtrace`:

.. doctest::

    julia> top_frame = stacktrace()[1]
     in eval(::Module, ::Any) at boot.jl:237

    julia> top_frame.func
    :eval

    julia> top_frame.file
    Symbol("./boot.jl")

    julia> top_frame.line
    237

    julia> top_frame.inlined_file
    Symbol("")

    julia> top_frame.inlined_line
    -1

    julia> top_frame.from_c
    false

::

    julia> top_frame.pointer
    13203085684

This makes stack trace information available programmatically for logging, error handling,
and more.

Error handling
--------------

While having easy access to information about the current state of the callstack can be
helpful in many places, the most obvious application is in error handling and debugging.

.. doctest::

    julia> @noinline bad_function() = undeclared_variable
    bad_function (generic function with 1 method)

    julia> @noinline example1() = try
               bad_function()
           catch
               stacktrace()
           end
    example1 (generic function with 1 method)

    julia> example1()
    ...-element Array{StackFrame,1}:
      [inlined code] from stacktraces.jl:135
     in example1() at none:2
      in eval(::Module, ::Any) at boot.jl:237
      [inlined code] from sysimg.jl:11
     ...

You may notice that in the example above the first stack frame points points at line 4,
where :func:`stacktrace` is called, rather than line 2, where `bad_function` is called, and
``bad_function``'s frame is missing entirely. This is understandable, given that
:func:`stacktrace` is called from the context of the `catch`. While in this example it's
fairly easy to find the actual source of the error, in complex cases tracking down the
source of the error becomes nontrivial.

This can be remedied by calling :func:`catch_stacktrace` instead of :func:`stacktrace`.
Instead of returning callstack information for the current context, :func:`catch_stacktrace`
returns stack information for the context of the most recent exception:

.. doctest::

    julia> @noinline bad_function1() = undeclared_variable
    bad_function1 (generic function with 1 method)

    julia> @noinline example2() = try
               bad_function1()
           catch
               catch_stacktrace()
           end
    example2 (generic function with 1 method)

    julia> example2()
    ...-element Array{StackFrame,1}:
      in bad_function1() at none:1
      in example2() at none:2
      in eval(::Module, ::Any) at boot.jl:237
      [inlined code] from sysimg.jl:11
     ...

Notice that the stack trace now indicates the appropriate line number and the missing frame.

.. doctest::

    julia> @noinline child1() = error("Whoops!")
    child1 (generic function with 1 method)

    julia> @noinline parent1() = child1()
    parent1 (generic function with 1 method)

    julia> @noinline function grandparent1()
               try
                   parent1()
               catch err
                   println("ERROR: ", err.msg)
                   catch_stacktrace()
               end
           end
    grandparent1 (generic function with 1 method)

    julia> grandparent1()
    ERROR: Whoops!
    ...-element Array{StackFrame,1}:
      in child1() at none:1
      in parent1() at none:1
      in grandparent1() at none:3
      in eval(::Module, ::Any) at boot.jl:237
      [inlined code] from sysimg.jl:11
     ...

Comparison with :func:`backtrace`
---------------------------------

A call to :func:`backtrace` returns a vector of ``Ptr{Void}``, which may then be passed into
:func:`stacktrace` for translation::

    julia> trace = backtrace()
    15-element Array{Ptr{Void},1}:
     Ptr{Void} @0x000000010527895e
     Ptr{Void} @0x0000000309bf6220
     Ptr{Void} @0x0000000309bf61a0
     Ptr{Void} @0x00000001052733b4
     Ptr{Void} @0x0000000105271a0e
     Ptr{Void} @0x000000010527189d
     Ptr{Void} @0x0000000105272e6d
     Ptr{Void} @0x0000000105272cef
     Ptr{Void} @0x0000000105285b88
     Ptr{Void} @0x000000010526b50e
     Ptr{Void} @0x000000010663cc28
     Ptr{Void} @0x0000000309bbc20f
     Ptr{Void} @0x0000000309bbbde7
     Ptr{Void} @0x0000000309bb0262
     Ptr{Void} @0x000000010527980e

    julia> stacktrace(trace)
    4-element Array{StackFrame,1}:
     backtrace at error.jl:26
     eval at boot.jl:265
     [inlined code from REPL.jl:3] eval_user_input at REPL.jl:62
     [inlined code from REPL.jl:92] anonymous at task.jl:63

Notice that the vector returned by :func:`backtrace` had 15 pointers, while the vector returned by :func:`stacktrace` only has 4. This is because, by default, :func:`stacktrace` removes any lower-level C functions from the stack. If you want to include stack frames from C calls, you can do it like this::

    julia> stacktrace(stack, true)
    15-element Array{StackFrame,1}:
     [inlined code from task.c:651] rec_backtrace at task.c:711
     backtrace at error.jl:26
     jlcall_backtrace_23146 at :-1
     [inlined code from interpreter.c:55] jl_apply at interpreter.c:65
     eval at interpreter.c:214
     eval at interpreter.c:220
     eval_body at interpreter.c:601
     jl_toplevel_eval_body at interpreter.c:534
     jl_toplevel_eval_flex at toplevel.c:525
     jl_toplevel_eval_in_warn at builtins.c:590
     eval at boot.jl:265
     [inlined code from REPL.jl:3] eval_user_input at REPL.jl:62
     jlcall_eval_user_input_22658 at :-1
     [inlined code from REPL.jl:92] anonymous at task.jl:63
     [inlined code from julia.h:1352] jl_apply at task.c:246

Individual pointers returned by :func:`backtrace` can be translated into :obj:`StackFrame` s
by passing them into :func:`StackTraces.lookup`:

.. doctest::

    julia> pointer = backtrace()[1]
    Ptr{Void} @0x...

    julia> frame = StackTraces.lookup(pointer)
     in jl_backtrace_from_here at stackwalk.c:103

    julia> println("The top frame is from $(frame.func)!")
    The top frame is from jl_backtrace_from_here!
