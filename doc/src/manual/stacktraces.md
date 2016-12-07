# Stack Traces

The `StackTraces` module provides simple stack traces that are both human readable and
easy to use programmatically.

## Viewing a stack trace

The primary function used to obtain a stack trace is [`stacktrace()`](@ref):

```julia
julia> stacktrace()
3-element Array{StackFrame,1}:
 eval at boot.jl:265
 [inlined code from REPL.jl:3] eval_user_input at REPL.jl:62
 [inlined code from REPL.jl:92] anonymous at task.jl:63
```

Calling [`stacktrace()`](@ref) returns a vector of [`StackFrame`](@ref) s. For ease of use, the
alias [`StackTrace`](@ref) can be used in place of `Vector{StackFrame}`. (Examples with `...`
indicate that output may vary depending on how the code is run.)

```julia
julia> example() = stacktrace()
example (generic function with 1 method)

julia> example()
11-element Array{StackFrame,1}:
  in example() at none:1
  in eval(::Module, ::Any) at boot.jl:236
  ...

julia> @noinline child() = stacktrace()
child (generic function with 1 method)

julia> @noinline parent() = child()
parent (generic function with 1 method)

julia> grandparent() = parent()
grandparent (generic function with 1 method)

julia> grandparent()
13-element Array{StackFrame,1}:
  in child() at none:1
  in parent() at none:1
  in grandparent() at none:1
  ...
```

Note that when calling [`stacktrace()`](@ref) you'll typically see a frame with `eval(...) at boot.jl`.
When calling [`stacktrace()`](@ref) from the REPL you'll also have a few extra frames in the stack
from `REPL.jl`, usually looking something like this:

```julia
julia> example() = stacktrace()
example (generic function with 1 method)

julia> example()
5-element Array{StackFrame,1}:
  in example() at REPL[1]:1
  in eval(::Module, ::Any) at boot.jl:234
  in eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:62
  in macro expansion at REPL.jl:92 [inlined]
  in (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:46
```

## Extracting useful information

Each [`StackFrame`](@ref) contains the function name, file name, line number, lambda info, a flag
indicating whether the frame has been inlined, a flag indicating whether it is a C function (by
default C functions do not appear in the stack trace), and an integer representation of the pointer
returned by [`backtrace()`](@ref):

```julia
julia> top_frame = stacktrace()[1]
 in eval(::Module, ::Any) at boot.jl:236

julia> top_frame.func
:eval

julia> top_frame.file
Symbol("./boot.jl")

julia> top_frame.line
236

julia> top_frame.linfo
Nullable{Core.MethodInstance}(MethodInstance for eval(::Module, ::Any))

julia> top_frame.inlined
false

julia> top_frame.from_c
false
```

```julia
julia> top_frame.pointer
13203085684
```

This makes stack trace information available programmatically for logging, error handling, and
more.

## Error handling

While having easy access to information about the current state of the callstack can be helpful
in many places, the most obvious application is in error handling and debugging.

```julia
julia> @noinline bad_function() = undeclared_variable
bad_function (generic function with 1 method)

julia> @noinline example() = try
           bad_function()
       catch
           stacktrace()
       end
example (generic function with 1 method)

julia> example()
11-element Array{StackFrame,1}:
  in example() at none:4
  in eval(::Module, ::Any) at boot.jl:236
  ...
```

You may notice that in the example above the first stack frame points points at line 4, where
[`stacktrace()`](@ref) is called, rather than line 2, where *bad_function* is called, and `bad_function`'s
frame is missing entirely. This is understandable, given that [`stacktrace()`](@ref) is called
from the context of the *catch*. While in this example it's fairly easy to find the actual source
of the error, in complex cases tracking down the source of the error becomes nontrivial.

This can be remedied by calling [`catch_stacktrace()`](@ref) instead of [`stacktrace()`](@ref).
Instead of returning callstack information for the current context, [`catch_stacktrace()`](@ref)
returns stack information for the context of the most recent exception:

```julia
julia> @noinline bad_function() = undeclared_variable
bad_function (generic function with 1 method)

julia> @noinline example() = try
           bad_function()
       catch
           catch_stacktrace()
       end
example (generic function with 1 method)

julia> example()
12-element Array{StackFrame,1}:
  in bad_function() at none:1
  in example() at none:2
  ...
```

Notice that the stack trace now indicates the appropriate line number and the missing frame.

```julia
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
13-element Array{StackFrame,1}:
  in child() at none:1
  in parent() at none:1
  in grandparent() at none:3
  ...
```

## Comparison with [`backtrace()`](@ref)

A call to [`backtrace()`](@ref) returns a vector of `Ptr{Void}`, which may then be passed into
[`stacktrace()`](@ref) for translation:

```julia
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
```

Notice that the vector returned by [`backtrace()`](@ref) had 15 pointers, while the vector returned
by [`stacktrace()`](@ref) only has 4. This is because, by default, [`stacktrace()`](@ref) removes
any lower-level C functions from the stack. If you want to include stack frames from C calls,
you can do it like this:

```julia
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
```

Individual pointers returned by [`backtrace()`](@ref) can be translated into [`StackFrame`](@ref)
s by passing them into [`StackTraces.lookup()`](@ref):

```julia
julia> pointer = backtrace()[1];

julia> frame = StackTraces.lookup(pointer)
1-element Array{StackFrame,1}:
  in jl_backtrace_from_here at stackwalk.c:105

julia> println("The top frame is from $(frame[1].func)!")
The top frame is from jl_backtrace_from_here!
```
