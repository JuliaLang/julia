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
    6-element Array{StackFrame,1}:
      in example() at none:1
      in eval(::Module, ::Any) at boot.jl:234
      in eval_user_input(::Any, ::Bool) at client.jl:117
      in eval(::Module, ::Any) at boot.jl:234
      in eval_user_input(::Any, ::Bool) at client.jl:117
      in _start() at client.jl:363

    julia> @noinline child() = stacktrace()
    child (generic function with 1 method)

    julia> @noinline parent() = child()
    parent (generic function with 1 method)

    julia> grandparent() = parent()
    grandparent (generic function with 1 method)

    julia> grandparent()
    8-element Array{StackFrame,1}:
      in child() at none:1
      in parent() at none:1
      in grandparent() at none:1
      ...

Note that when calling :func:`stacktrace` you'll typically see a frame with
``eval(...) at boot.jl``. When calling :func:`stacktrace` from the REPL you'll also have a few
extra frames in the stack from ``REPL.jl``, usually looking something like this::

    julia> example() = stacktrace()
    example (generic function with 1 method)

    julia> example()
    5-element Array{StackFrame,1}:
      in example() at REPL[1]:1
      in eval(::Module, ::Any) at boot.jl:234
      in eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:62
      in macro expansion at REPL.jl:92 [inlined]
      in (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:46

Extracting useful information
-----------------------------

Each :obj:`StackFrame` contains the function name, file name, line number, lambda info, a flag indicating whether the frame has been inlined, a flag indicating whether it is a C function (by default C functions do not appear in the stack trace), and an integer representation of the pointer returned by :func:`backtrace`:

.. doctest::

    julia> top_frame = stacktrace()[1]
     in eval(::Module, ::Any) at boot.jl:234

    julia> top_frame.func
    :eval

    julia> top_frame.file
    Symbol("./boot.jl")

    julia> top_frame.line
    234

    julia> top_frame.linfo
    Nullable{LambdaInfo}(LambdaInfo for eval(::Module, ::Any))

    julia> top_frame.inlined
    false

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

.. doctest:: error-handling

    julia> @noinline bad_function() = undeclared_variable
    bad_function (generic function with 1 method)

    julia> @noinline example() = try
               bad_function()
           catch
               stacktrace()
           end
    example (generic function with 1 method)

    julia> example()
    6-element Array{StackFrame,1}:
      in example() at none:4
      in eval(::Module, ::Any) at boot.jl:234
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

.. doctest:: catch-stacktrace

    julia> @noinline bad_function() = undeclared_variable
    bad_function (generic function with 1 method)

    julia> @noinline example() = try
               bad_function()
           catch
               catch_stacktrace()
           end
    example (generic function with 1 method)

    julia> example()
    7-element Array{StackFrame,1}:
      in bad_function() at none:1
      in example() at none:2
      ...

Notice that the stack trace now indicates the appropriate line number and the missing frame.

.. doctest:: catch-stacktrace-demo

    julia> @noinline child() = error("Whoops!")
    child (generic function with 1 method)

    julia> @noinline parent() = child()
    parent (generic function with 1 method)

    julia> @noinline function grandparent()
               try
                   parent()
               catch err
                   println("ERROR: ", err.msg)
                   catch_stacktrace()
               end
           end
    grandparent (generic function with 1 method)

    julia> grandparent()
    ERROR: Whoops!
    8-element Array{StackFrame,1}:
      in child() at none:1
      in parent() at none:1
      in grandparent() at none:3
      ...

Comparison with :func:`backtrace`
---------------------------------

A call to :func:`backtrace` returns a vector of ``Ptr{Void}``, which may then be passed into
:func:`stacktrace` for translation::

    julia> trace = backtrace()
    20-element Array{Ptr{Void},1}:
     Ptr{Void} @0x0000000100a26fc2
     Ptr{Void} @0x00000001029435df
     Ptr{Void} @0x0000000102943635
     Ptr{Void} @0x00000001009e9620
     Ptr{Void} @0x00000001009fe1e8
     Ptr{Void} @0x00000001009fc7b6
     Ptr{Void} @0x00000001009fdae3
     Ptr{Void} @0x00000001009fe0d2
     Ptr{Void} @0x0000000100a1321b
     Ptr{Void} @0x00000001009f64e7
     Ptr{Void} @0x000000010265ac5d
     Ptr{Void} @0x000000010265acc1
     Ptr{Void} @0x00000001009e9620
     Ptr{Void} @0x000000031007744b
     Ptr{Void} @0x0000000310077537
     Ptr{Void} @0x00000001009e9620
     Ptr{Void} @0x000000031006feec
     Ptr{Void} @0x00000003100701b0
     Ptr{Void} @0x00000001009e9635
     Ptr{Void} @0x0000000100a06418

    julia> stacktrace(trace)
    5-element Array{StackFrame,1}:
      in backtrace() at error.jl:26
      in eval(::Module, ::Any) at boot.jl:231
      in eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:62
      in macro expansion at REPL.jl:92 [inlined]
      in (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:46

Notice that the vector returned by :func:`backtrace` had 15 pointers, while the vector returned by :func:`stacktrace` only has 4. This is because, by default, :func:`stacktrace` removes any lower-level C functions from the stack. If you want to include stack frames from C calls, you can do it like this::

    julia> stacktrace(trace, true)
    26-element Array{StackFrame,1}:
      in jl_backtrace_from_here at stackwalk.c:104
      in backtrace() at error.jl:26
      in ip:0x102943635
      in jl_call_method_internal at julia_internal.h:86 [inlined]
      in jl_apply_generic at gf.c:1805
      in do_call at interpreter.c:65
      in eval at interpreter.c:188
      in eval_body at interpreter.c:469
      in jl_interpret_call at interpreter.c:573
      in jl_toplevel_eval_flex at toplevel.c:543
      in jl_toplevel_eval_in_warn at builtins.c:571
      in eval(::Module, ::Any) at boot.jl:231
      in ip:0x10265acc1
      in jl_call_method_internal at julia_internal.h:86 [inlined]
      in jl_apply_generic at gf.c:1805
      in eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:62
      in ip:0x310077537
      in jl_call_method_internal at julia_internal.h:86 [inlined]
      in jl_apply_generic at gf.c:1805
      in macro expansion at REPL.jl:92 [inlined]
      in (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:46
      in ip:0x3100701b0
      in jl_call_method_internal at julia_internal.h:86 [inlined]
      in jl_apply_generic at gf.c:1795
      in jl_apply at julia.h:1388 [inlined]
      in start_task at task.c:247

Individual pointers returned by :func:`backtrace` can be translated into :obj:`StackFrame` s
by passing them into :func:`StackTraces.lookup`:

.. doctest::

    julia> pointer = backtrace()[1];

    julia> frame = StackTraces.lookup(pointer)
    1-element Array{StackFrame,1}:
      in jl_backtrace_from_here at stackwalk.c:105

    julia> println("The top frame is from $(frame[1].func)!")
    The top frame is from jl_backtrace_from_here!
