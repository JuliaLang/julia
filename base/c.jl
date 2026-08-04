# This file is a part of Julia. License is MIT: https://julialang.org/license

# definitions related to C interface

"""
    CFunction struct

Garbage-collection handle for the return value from `@cfunction`
when the first argument is annotated with '\\\$'.
Like all `cfunction` handles, it should be passed to `ccall` as a `Ptr{Cvoid}`,
and will be converted automatically at the call site to the appropriate type.

See [`@cfunction`](@ref).
"""
mutable struct CFunction <: Ref{Cvoid}
    ptr::Ptr{Cvoid}
    f::Any
    _1::Ptr{Cvoid}
    _2::Ptr{Cvoid}
    let constructor = false end
end
unsafe_convert(::Type{Ptr{Cvoid}}, cf::CFunction) = cf.ptr

"""
    @cfunction(callable, ReturnType, (ArgumentTypes...,)) -> Ptr{Cvoid}
    @cfunction(\$callable, ReturnType, (ArgumentTypes...,)) -> CFunction

Generate a C-callable function pointer from the Julia function `callable`
for the given type signature.
To pass the return value to a `ccall`, use the argument type `Ptr{Cvoid}` in the signature.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression
(although it can include a splat expression). And that these arguments will be evaluated in global scope
during compile-time (not deferred until runtime).
Adding a '\\\$' in front of the function argument changes this to instead create a runtime closure
over the local variable `callable` (this is not supported on all architectures).

See [manual section on ccall and cfunction usage](@ref Calling-C-and-Fortran-Code).

# Examples
```julia-repl
julia> function foo(x::Int, y::Int)
           return x + y
       end

julia> @cfunction(foo, Int, (Int, Int))
Ptr{Cvoid} @0x000000001b82fcd0
```
"""
macro cfunction(f, rt, at)
    if !(isa(at, Expr) && at.head === :tuple)
        throw(ArgumentError("@cfunction argument types must be a literal tuple"))
    end
    at.head = :call
    pushfirst!(at.args, GlobalRef(Core, :svec))
    if isa(f, Expr) && f.head === :$
        fptr = f.args[1]
        typ = CFunction
    else
        fptr = QuoteNode(f)
        typ = Ptr{Cvoid}
    end
    cfun = Expr(:cfunction, typ, fptr, rt, at, QuoteNode(:ccall))
    return esc(cfun)
end

if ccall(:jl_is_char_signed, Ref{Bool}, ())
    const Cchar = Int8
else
    const Cchar = UInt8
end
"""
    Cchar

Equivalent to the native `char` c-type.
"""
Cchar

# The ccall here is equivalent to Sys.iswindows(), but that's not defined yet
if ccall(:jl_get_UNAME, Any, ()) === :NT
    const Clong = Int32
    const Culong = UInt32
    const Cwchar_t = UInt16
else
    const Clong = Int
    const Culong = UInt
    const Cwchar_t = Int32
end

"""
    Clong

Equivalent to the native `signed long` c-type.
"""
Clong

"""
    Culong

Equivalent to the native `unsigned long` c-type.
"""
Culong

"""
    Cwchar_t

Equivalent to the native `wchar_t` c-type ([`Int32`](@ref)).
"""
Cwchar_t

if ccall(:jl_get_UNAME, Any, ()) !== :NT
    const sizeof_mode_t = ccall(:jl_sizeof_mode_t, Cint, ())
    if sizeof_mode_t == 2
        const Cmode_t = Int16
    elseif sizeof_mode_t == 4
        const Cmode_t = Int32
    elseif sizeof_mode_t == 8
        const Cmode_t = Int64
    else
        error("invalid sizeof mode_t")
    end
end

# deferring (or un-deferring) ctrl-c handler for external C code that
# is not interrupt safe (see also issue #2622).  The sigatomic_begin/end
# functions should always be called in matched pairs, ideally via:
#            disable_sigint() do .. end
# reennable_sigint is provided so that immediate ctrl-c handling is
# re-enabled within a sigatomic region, e.g. inside a Julia callback function
# within a long-running C routine.
sigatomic_begin() = ccall(:jl_sigatomic_begin, Cvoid, ())
sigatomic_end() = ccall(:jl_sigatomic_end, Cvoid, ())

"""
    disable_sigint(f::Function)

Disable Ctrl-C handler during execution of a function on the current task,
for calling external code that may call julia code that is not interrupt safe.
Intended to be called using `do` block syntax as follows:

    disable_sigint() do
        # interrupt-unsafe code
        ...
    end

This is not needed on worker threads (`Threads.threadid() != 1`) since the
`InterruptException` will only be delivered to the master thread.
External functions that do not call julia code or julia runtime
automatically disable sigint during their execution.
"""
function disable_sigint(f::Function)
    sigatomic_begin()
    res = f()
    # Exception unwind sigatomic automatically
    sigatomic_end()
    res
end

"""
    reenable_sigint(f::Function)

Re-enable Ctrl-C handler during execution of a function.
Temporarily reverses the effect of [`disable_sigint`](@ref).
"""
function reenable_sigint(f::Function)
    sigatomic_end()
    res = f()
    # Exception unwind sigatomic automatically
    sigatomic_begin()
    res
end

"""
    exit_on_sigint(on::Bool)

Set `exit_on_sigint` flag of the julia runtime.  If `false`, Ctrl-C
(SIGINT) is capturable as [`InterruptException`](@ref) in `try` block.
This is the default behavior in REPL, any code run via `-e` and `-E`
and in Julia script run with `-i` option.

If `true`, `InterruptException` is not thrown by Ctrl-C.  Running code
upon such event requires [`atexit`](@ref).  This is the default
behavior in Julia script run without `-i` option.

!!! compat "Julia 1.5"
    Function `exit_on_sigint` requires at least Julia 1.5.
"""
function exit_on_sigint(on::Bool)
    ccall(:jl_exit_on_sigint, Cvoid, (Cint,), on)
end

function _ccallable(name::Union{Nothing, String}, rt::Type, sigt::Type)
    ccall(:jl_extern_c, Cvoid, (Any, Any, Any), name, rt, sigt)
end

function expand_ccallable(name, rt, def)
    if isa(def,Expr) && (def.head === :(=) || def.head === :function)
        sig = def.args[1]
        if sig.head === :(::)
            if rt === nothing
                rt = sig.args[2]
            end
            sig = sig.args[1]
        end
        if rt === nothing
            error("@ccallable requires a return type")
        end
        if sig.head === :call
            f = sig.args[1]
            if isa(f,Expr) && f.head === :(::)
                f = f.args[end]
            else
                f = :(typeof($f))
            end
            at = Any[let a = sig.args[i]
                    if isa(a,Expr) && a.head === :(::)
                        a.args[end]
                    else
                        :Any
                    end
                end for i in 2:length(sig.args)]
            return quote
                @__doc__ $(esc(def))
                _ccallable($name, $(esc(rt)), $(Expr(:curly, :Tuple, esc(f), map!(esc, at, at)...)))
            end
        end
    end
    error("expected method definition in @ccallable")
end

"""
    @ccallable ["name"] function f(...)::RetType ... end

Make the annotated function be callable from C using its name. This can, for example,
be used to expose functionality as a C API when creating a custom Julia sysimage.

If the first argument is a string, it is used as the external name of the function.
"""
macro ccallable(def)
    expand_ccallable(nothing, nothing, def)
end
macro ccallable(rt, def)
    if rt isa String
        expand_ccallable(rt, nothing, def)
    else
        expand_ccallable(nothing, rt, def)
    end
end

# @ccall implementation
"""
    ccall_macro_parse(expression)

`ccall_macro_parse` is an implementation detail of `@ccall`.

It takes an expression like `:(printf("%d"::Cstring, value::Cuint)::Cvoid)`
returns: a tuple of `(function_name, return_type, arg_types, args)`

The above input outputs this:

    (:printf, :Cvoid, [:Cstring, :Cuint], ["%d", :value])
"""
function ccall_macro_parse(exprs)
    gc_safe = false
    cancel = nothing
    expr = nothing
    if exprs isa Expr
        expr = exprs
    else
        # leading `name = value` options, then the call expression
        i = 1
        while i < length(exprs) && isexpr(exprs[i], :(=))
            opt = exprs[i]::Expr
            name = opt.args[1]
            value = opt.args[2]
            if name === :gc_safe
                if value === true
                    gc_safe = true
                elseif value === false
                    gc_safe = false
                else
                    throw(ArgumentError("gc_safe must be true or false"))
                end
            elseif name === :cancel_handler
                if !(isexpr(value, :tuple) && length(value.args) == 2)
                    throw(ArgumentError("cancel_handler must be a `(handler, state)` tuple"))
                end
                cancel = (value.args[1], value.args[2])
            else
                throw(ArgumentError("@ccall options are `gc_safe = <bool>` and `cancel_handler = (handler, state)`"))
            end
            i += 1
        end
        if i != length(exprs)
            throw(ArgumentError("@ccall needs a function signature with a return type"))
        end
        expr = exprs[i]
    end

    # setup and check for errors
    if !isexpr(expr, :(::))
        throw(ArgumentError("@ccall needs a function signature with a return type"))
    end
    rettype = expr.args[2]

    call = expr.args[1]
    if !isexpr(call, :call)
        throw(ArgumentError("@ccall has to take a function call"))
    end

    # get the function symbols
    func = let f = call.args[1]
        if isexpr(f, :.)
            Expr(:tuple, f.args[2], f.args[1])
        elseif isexpr(f, :$)
            func = f.args[1]
            if isa(func, String) || (isa(func, QuoteNode) && !isa(func.value, Ptr)) || isa(func, Tuple) || isexpr(func, :tuple)
                throw(ArgumentError("interpolated value should be a variable or expression, not a literal name or tuple"))
            end
            func
        elseif f isa Symbol
            Expr(:tuple, QuoteNode(f))
        else
            throw(ArgumentError("@ccall function name must be a symbol, a `.` node (e.g. `libc.printf`) or an interpolated function pointer (with `\$`)"))
        end
    end

    # detect varargs
    varargs = nothing
    argstart = 2
    callargs = call.args
    if length(callargs) >= 2 && isexpr(callargs[2], :parameters)
        argstart = 3
        varargs = callargs[2].args
    end

    # collect args and types
    args = []
    types = []

    function pusharg!(arg)
        if !isexpr(arg, :(::))
            throw(ArgumentError("args in @ccall need type annotations. '$arg' doesn't have one."))
        end
        push!(args, arg.args[1])
        push!(types, arg.args[2])
    end

    for i in argstart:length(callargs)
        pusharg!(callargs[i])
    end
    # add any varargs if necessary
    nreq = 0
    if varargs !== nothing
        if length(args) == 0
            throw(ArgumentError("C ABI prohibits vararg without one required argument"))
        end
        nreq = length(args)
        for a in varargs
            pusharg!(a)
        end
    end
    return func, rettype, types, args, gc_safe, cancel, nreq
end


function ccall_macro_lower(convention, func, rettype, types, args, gc_safe, cancel, nreq)
    have_cancel = cancel !== nothing
    base_cconv = convention isa Tuple ? (convention..., gc_safe) :
                                        (convention, UInt16(0), gc_safe)
    if !have_cancel
        cconv = Expr(:cconv, base_cconv, nreq)
        return Expr(:call, :ccall, esc(func), cconv, esc(rettype),
                     Expr(:tuple, map!(esc, types, types)...), map!(esc, args, args)...)
    end
    # Cancellation-handler annotation: the (handler, state) pair rides as
    # two extra leading foreigncall arguments - peeled off again by codegen,
    # which publishes them as the handler context around the call. The
    # annotation implies no cancellation point: binding the governing source
    # (which gates delivery) and observing the cancellation itself are the
    # caller's business - place a `@cancel_check` before the call, and after
    # it either re-check or have the handler make the foreign call return a
    # recognizable abort code.
    fex, sex = cancel
    nreq > 0 && (nreq += 2)
    cconv = Expr(:cconv, (base_cconv..., true), nreq)
    return Expr(:call, :ccall, esc(func), cconv, esc(rettype),
                Expr(:tuple, :(Ptr{Cvoid}), :(Ptr{Cvoid}), map!(esc, types, types)...),
                esc(fex), esc(sex), map!(esc, args, args)...)
end

"""
    @ccall library.function_name(argvalue1::argtype1, ...)::returntype
    @ccall function_name(argvalue1::argtype1, ...)::returntype
    @ccall \$function_pointer(argvalue1::argtype1, ...)::returntype

Call a function in a C-exported shared library, specified by
`library.function_name`, where `library` is a string constant or
literal. The library may be omitted, in which case the `function_name`
is resolved in the current process. Alternatively, `@ccall` may
also be used to call a function pointer `\$function_pointer`, such as
one returned by `dlsym`.

Each `argvalue` to `@ccall` is converted to the corresponding
`argtype`, by automatic insertion of calls to `unsafe_convert(argtype,
cconvert(argtype, argvalue))`. (See also the documentation for
[`unsafe_convert`](@ref Base.unsafe_convert) and [`cconvert`](@ref
Base.cconvert) for further details.) In most cases, this simply
results in a call to `convert(argtype, argvalue)`.


# Examples

    @ccall strlen(s::Cstring)::Csize_t

This calls the C standard library function:

    size_t strlen(char *)

with a Julia variable named `s`. See also `ccall`.

Varargs are supported with the following convention:

    @ccall printf("%s = %d"::Cstring ; "foo"::Cstring, foo::Cint)::Cint

The semicolon is used to separate required arguments (of which there
must be at least one) from variadic arguments.

Example using an external library:

    # C signature of g_uri_escape_string:
    # char *g_uri_escape_string(const char *unescaped, const char *reserved_chars_allowed, gboolean allow_utf8);

    const glib = "libglib-2.0"
    @ccall glib.g_uri_escape_string(my_uri::Cstring, ":/"::Cstring, true::Cint)::Cstring

The string literal could also be used directly before the function
name, if desired `"libglib-2.0".g_uri_escape_string(...`

It's possible to declare the ccall as `gc_safe` by using the `gc_safe = true` option:

    @ccall gc_safe=true strlen(s::Cstring)::Csize_t

This allows the garbage collector to run concurrently with the ccall, which can be useful whenever
the `ccall` may block outside of julia.

!!! warning
    This option should be used with caution, as it can lead to undefined behavior if the ccall
    calls back into the julia runtime. (`@cfunction`/`@ccallable` are safe however)

!!! compat "Julia 1.12"
    The `gc_safe` argument requires Julia 1.12 or higher.

A long-running foreign call can be made cancellable with the
`cancel_handler = (handler, state)` option:

    @ccall cancel_handler=(CANCEL_FPTR, ref) lib.solve(ref::Ptr{Cvoid})::Cvoid

`handler` is a C-callable function pointer `void (*)(void *state, uint8_t
sev)` (typically from [`@cfunction`](@ref)). If the cancellation token
source (`Base.CancellationTokenSource`) bound to the calling task is
cancelled while the call runs, the runtime invokes `handler(state, sev)` *on
the thread executing the call*, like a signal handler: at an arbitrary point
of the foreign code, on the same stack, resuming the interrupted call when
the handler returns (`sev` is the request state of the cancelled source).
The handler performs the library-specific work to make the foreign call
return early - typically setting a flag or calling the library's own
cancellation entry point.

The annotation implies no cancellation point of its own. Delivery is gated
on the task's *bound* token source, which is what the most recent
cancellation point bound - so place a `Base.@cancel_check` before the
call (it also throws a cancellation that is already pending, which the
handler cannot abort). How the aborted call's result is handled afterwards
is equally the caller's choice: the handler can make the foreign function
return a recognizable abort code that the caller inspects, or the caller
can simply re-check with `Base.@cancel_check` after the call, which
throws the delivered cancellation before a partial result can escape.

The whole handler interface is a plain C ABI: the runtime does not assume it
is safe to run Julia code at the interrupted point (a handler that knows
better can still be written in Julia via [`@cfunction`](@ref)). `state`
undergoes the standard `ccall` argument conversion to `Ptr{Cvoid}`
(`unsafe_convert(Ptr{Cvoid}, cconvert(Ptr{Cvoid}, state))`), and, like any
other argument, the converted value is kept rooted for the duration of the
call - pass a `Ref` to hand the handler a pointer to Julia-managed data.
Handler restrictions mirror those of a signal handler: it must not allocate,
yield, perform I/O, take locks, or throw, and it must tolerate being invoked
multiple times (once per redelivery), spuriously, and concurrently with the
call completing. The delivery machinery preserves the complete interrupted
register state around the handler, so it may otherwise do anything an
ordinary C function may. The handler is only invoked while the call is
actually running on a thread (at most one invocation at a time per thread);
delivery is best-effort and the cancellation itself remains level-triggered.

!!! compat "Julia 1.14"
    The `cancel_handler` option requires Julia 1.14 or higher.

"""
macro ccall(exprs...)
    return ccall_macro_lower((:ccall), ccall_macro_parse(exprs)...)
end

macro ccall_effects(effects::UInt16, exprs...)
    return ccall_macro_lower((:ccall, effects), ccall_macro_parse(exprs)...)
end
