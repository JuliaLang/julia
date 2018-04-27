# Stack Traces

The `StackTraces` module provides simple stack traces that are both human readable and
easy to use programmatically.

## Viewing a stack trace

The primary function used to obtain a stack trace is [`stacktrace`](@ref):

```julia-repl
julia> stacktrace()
4-element Array{StackFrame,1}:
 eval(::Module, ::Any) at boot.jl:236
 eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:66
 macro expansion at REPL.jl:97 [inlined]
 (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:73
```

Calling [`stacktrace()`](@ref) returns a vector of [`StackTraces.StackFrame`](@ref) s. For ease of use, the
alias [`StackTraces.StackTrace`](@ref) can be used in place of `Vector{StackFrame}`. (Examples with `[...]`
indicate that output may vary depending on how the code is run.)

```julia-repl
julia> example() = stacktrace()
example (generic function with 1 method)

julia> example()
5-element Array{StackFrame,1}:
 example() at REPL[1]:1
 eval(::Module, ::Any) at boot.jl:236
[...]

julia> @noinline child() = stacktrace()
child (generic function with 1 method)

julia> @noinline parent() = child()
parent (generic function with 1 method)

julia> grandparent() = parent()
grandparent (generic function with 1 method)

julia> grandparent()
7-element Array{StackFrame,1}:
 child() at REPL[3]:1
 parent() at REPL[4]:1
 grandparent() at REPL[5]:1
[...]
```

Note that when calling [`stacktrace()`](@ref) you'll typically see a frame with `eval(...) at boot.jl`.
When calling [`stacktrace()`](@ref) from the REPL you'll also have a few extra frames in the stack
from `REPL.jl`, usually looking something like this:

```julia-repl
julia> example() = stacktrace()
example (generic function with 1 method)

julia> example()
5-element Array{StackFrame,1}:
 example() at REPL[1]:1
 eval(::Module, ::Any) at boot.jl:236
 eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:66
 macro expansion at REPL.jl:97 [inlined]
 (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:73
```

## Extracting useful information

Each [`StackTraces.StackFrame`](@ref) contains the function name, file name, line number, lambda info, a flag
indicating whether the frame has been inlined, a flag indicating whether it is a C function (by
default C functions do not appear in the stack trace), and an integer representation of the pointer
returned by [`backtrace`](@ref):

```julia-repl
julia> top_frame = stacktrace()[1]
eval(::Module, ::Expr) at REPL.jl:3

julia> top_frame.func
:eval

julia> top_frame.file
Symbol("./boot.jl")

julia> top_frame.line
236

julia> top_frame.linfo
MethodInstance for eval(::Module, ::Expr)

julia> top_frame.inlined
false

julia> top_frame.from_c
false
```

```julia-repl
julia> top_frame.pointer
0x00007f390d152a59
```

This makes stack trace information available programmatically for logging, error handling, and
more.

## Error handling

While having easy access to information about the current state of the callstack can be helpful
in many places, the most obvious application is in error handling and debugging.

```julia-repl
julia> @noinline bad_function() = undeclared_variable
bad_function (generic function with 1 method)

julia> @noinline example() = try
           bad_function()
       catch
           stacktrace()
       end
example (generic function with 1 method)

julia> example()
5-element Array{StackFrame,1}:
 example() at REPL[2]:4
 eval(::Module, ::Any) at boot.jl:236
[...]
```

You may notice that in the example above the first stack frame points points at line 4, where
[`stacktrace`](@ref) is called, rather than line 2, where *bad_function* is called, and `bad_function`'s
frame is missing entirely. This is understandable, given that [`stacktrace`](@ref) is called
from the context of the *catch*. While in this example it's fairly easy to find the actual source
of the error, in complex cases tracking down the source of the error becomes nontrivial.

This can be remedied by passing the result of [`catch_backtrace`](@ref) to [`stacktrace`](@ref).
Instead of returning callstack information for the current context, [`catch_backtrace`](@ref)
returns stack information for the context of the most recent exception:

```julia-repl
julia> @noinline bad_function() = undeclared_variable
bad_function (generic function with 1 method)

julia> @noinline example() = try
           bad_function()
       catch
           stacktrace(catch_backtrace())
       end
example (generic function with 1 method)

julia> example()
6-element Array{StackFrame,1}:
 bad_function() at REPL[1]:1
 example() at REPL[2]:2
[...]
```

Notice that the stack trace now indicates the appropriate line number and the missing frame.

```julia-repl
julia> @noinline child() = error("Whoops!")
child (generic function with 1 method)

julia> @noinline parent() = child()
parent (generic function with 1 method)

julia> @noinline function grandparent()
           try
               parent()
           catch err
               println("ERROR: ", err.msg)
               stacktrace(catch_backtrace())
           end
       end
grandparent (generic function with 1 method)

julia> grandparent()
ERROR: Whoops!
7-element Array{StackFrame,1}:
 child() at REPL[1]:1
 parent() at REPL[2]:1
 grandparent() at REPL[3]:3
[...]
```

## Comparison with [`backtrace`](@ref)

A call to [`backtrace`](@ref) returns a vector of `Ptr{Cvoid}`, which may then be passed into
[`stacktrace`](@ref) for translation:

```julia-repl
julia> trace = backtrace()
21-element Array{Ptr{Cvoid},1}:
 Ptr{Cvoid} @0x00007f10049d5b2f
 Ptr{Cvoid} @0x00007f0ffeb4d29c
 Ptr{Cvoid} @0x00007f0ffeb4d2a9
 Ptr{Cvoid} @0x00007f1004993fe7
 Ptr{Cvoid} @0x00007f10049a92be
 Ptr{Cvoid} @0x00007f10049a823a
 Ptr{Cvoid} @0x00007f10049a9fb0
 Ptr{Cvoid} @0x00007f10049aa718
 Ptr{Cvoid} @0x00007f10049c0d5e
 Ptr{Cvoid} @0x00007f10049a3286
 Ptr{Cvoid} @0x00007f0ffe9ba3ba
 Ptr{Cvoid} @0x00007f0ffe9ba3d0
 Ptr{Cvoid} @0x00007f1004993fe7
 Ptr{Cvoid} @0x00007f0ded34583d
 Ptr{Cvoid} @0x00007f0ded345a87
 Ptr{Cvoid} @0x00007f1004993fe7
 Ptr{Cvoid} @0x00007f0ded34308f
 Ptr{Cvoid} @0x00007f0ded343320
 Ptr{Cvoid} @0x00007f1004993fe7
 Ptr{Cvoid} @0x00007f10049aeb67
 Ptr{Cvoid} @0x0000000000000000

julia> stacktrace(trace)
5-element Array{StackFrame,1}:
 backtrace() at error.jl:46
 eval(::Module, ::Any) at boot.jl:236
 eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:66
 macro expansion at REPL.jl:97 [inlined]
 (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:73
```

Notice that the vector returned by [`backtrace`](@ref) had 21 pointers, while the vector returned
by [`stacktrace`](@ref) only has 5. This is because, by default, [`stacktrace`](@ref) removes
any lower-level C functions from the stack. If you want to include stack frames from C calls,
you can do it like this:

```julia-repl
julia> stacktrace(trace, true)
27-element Array{StackFrame,1}:
 jl_backtrace_from_here at stackwalk.c:103
 backtrace() at error.jl:46
 backtrace() at sys.so:?
 jl_call_method_internal at julia_internal.h:248 [inlined]
 jl_apply_generic at gf.c:2215
 do_call at interpreter.c:75
 eval at interpreter.c:215
 eval_body at interpreter.c:519
 jl_interpret_toplevel_thunk at interpreter.c:664
 jl_toplevel_eval_flex at toplevel.c:592
 jl_toplevel_eval_in at builtins.c:614
 eval(::Module, ::Any) at boot.jl:236
 eval(::Module, ::Any) at sys.so:?
 jl_call_method_internal at julia_internal.h:248 [inlined]
 jl_apply_generic at gf.c:2215
 eval_user_input(::Any, ::Base.REPL.REPLBackend) at REPL.jl:66
 ip:0x7f1c707f1846
 jl_call_method_internal at julia_internal.h:248 [inlined]
 jl_apply_generic at gf.c:2215
 macro expansion at REPL.jl:97 [inlined]
 (::Base.REPL.##1#2{Base.REPL.REPLBackend})() at event.jl:73
 ip:0x7f1c707ea1ef
 jl_call_method_internal at julia_internal.h:248 [inlined]
 jl_apply_generic at gf.c:2215
 jl_apply at julia.h:1411 [inlined]
 start_task at task.c:261
 ip:0xffffffffffffffff
```

Individual pointers returned by [`backtrace`](@ref) can be translated into [`StackTraces.StackFrame`](@ref)
s by passing them into [`StackTraces.lookup`](@ref):

```julia-repl
julia> pointer = backtrace()[1];

julia> frame = StackTraces.lookup(pointer)
1-element Array{StackFrame,1}:
 jl_backtrace_from_here at stackwalk.c:103

julia> println("The top frame is from $(frame[1].func)!")
The top frame is from jl_backtrace_from_here!
```
