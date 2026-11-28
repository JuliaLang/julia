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
    @cfunction(callable, ReturnType, (ArgumentTypes...,), runtime_free=true) -> Ptr{Cvoid}

Generate a C-callable function pointer from the Julia function `callable`
for the given type signature.
To pass the return value to a `ccall`, use the argument type `Ptr{Cvoid}` in the signature.

Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression
(although it can include a splat expression). And that these arguments will be evaluated in global scope
during compile-time (not deferred until runtime).
Adding a '\\\$' in front of the function argument changes this to instead create a runtime closure
over the local variable `callable` (this is not supported on all architectures).

See [manual section on ccall and cfunction usage](@ref Calling-C-and-Fortran-Code).

# Runtime-free callbacks

A C function pointer from `@cfunction` enters the Julia runtime when it is called: it
makes the thread that calls it known to Julia if it is not, may wait there for the garbage
collector, and calls the method of `callable` that is the latest at that time. So C must
not call it where Julia code cannot run: while the garbage collector runs, in a signal
handler, or in the parts of the runtime that must not reach a safepoint.

With `runtime_free=true`, the function pointer is instead for code that does not use the
Julia runtime at all, so that C may call it at any time, on any thread. That code is
`callable`'s method and everything that it calls, as compiled when `@cfunction` is
evaluated: a later definition of a method does not change what an existing pointer calls,
but only what evaluating `@cfunction` again returns. An error that says what needs the
runtime is thrown (when `@cfunction` is evaluated) unless all of this holds:

* The argument types and the return type are `isbits` types, `callable` is not a closure,
  and it is inferred to return a value of exactly `ReturnType`.
* The code does not allocate, and cannot throw: there is no caller that could catch an
  exception. Use `@inbounds` and unchecked arithmetic where the compiler cannot prove an
  error impossible. (With `--check-bounds=yes`, an index is always checked, so only code
  that has none can be compiled.)
* Every call in it is resolved statically, without dynamic dispatch, and it has no `ccall`,
  `try` or task-local state: it consists of arithmetic, control flow and loads and stores
  of memory.

That goes for the functions that `callable` calls as it does for `callable`. They do not
have to be inlined: all of that code is compiled for the callback, as one closed set, apart
from the code that Julia otherwise runs for these functions.

It is then up to that code to only use memory that stays valid without the garbage
collector's cooperation: what its arguments point to, and constants, such as a `const`
tuple of numbers.

!!! compat "Julia 1.15"
    The `runtime_free` option requires Julia 1.15 or higher.

# Examples
```julia-repl
julia> function foo(x::Int, y::Int)
           return x + y
       end

julia> @cfunction(foo, Int, (Int, Int))
Ptr{Cvoid} @0x000000001b82fcd0

julia> ccall(@cfunction(foo, Int, (Int, Int), runtime_free=true), Int, (Int, Int), 1, 2)
3
```
"""
macro cfunction(f, rt, at, options...)
    if !(isa(at, Expr) && at.head === :tuple)
        throw(ArgumentError("@cfunction argument types must be a literal tuple"))
    end
    runtime_free = false
    for opt in options
        if isa(opt, Expr) && (opt.head === :(=) || opt.head === :kw) && length(opt.args) == 2 &&
                opt.args[1] === :runtime_free && opt.args[2] isa Bool
            runtime_free = opt.args[2]
        else
            throw(ArgumentError("@cfunction options are `runtime_free = <bool>`"))
        end
    end
    at.head = :call
    pushfirst!(at.args, GlobalRef(Core, :svec))
    if isa(f, Expr) && f.head === :$
        runtime_free && throw(ArgumentError("@cfunction: a runtime-free callback cannot be a closure"))
        fptr = f.args[1]
        typ = CFunction
    else
        fptr = QuoteNode(f)
        typ = Ptr{Cvoid}
    end
    # the calling convention is `ccall`, or `runtime_free` for a wrapper
    # that does not enter the runtime
    cfun = Expr(:cfunction, typ, fptr, rt, at, QuoteNode(runtime_free ? :runtime_free : :ccall))
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

Disable Ctrl-C handler during execution of a function,
for calling external code that may call julia code that is not interrupt safe.
Intended to be called using `do` block syntax as follows:

    disable_sigint() do
        # interrupt-unsafe code
        ...
    end

Within `f`, the scoped cancellation token ([`Base.CANCEL_TOKEN`](@ref)) is
`nothing`, so neither Ctrl-C nor any other cancellation of the enclosing
computation unwinds `f`: it is observed at the first cancellation point after
`f` returns. Tasks started within `f` inherit this.

External functions that do not call julia code or julia runtime
automatically disable sigint during their execution.
"""
function disable_sigint(f::Function)
    sigatomic_begin()
    res = ScopedValues.with(f, CANCEL_TOKEN => nothing, _sigint_shielded_token => CANCEL_TOKEN[])
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
    # a bare sigatomic region shields no token
    shielded = ScopedValues.get(_sigint_shielded_token)
    res = shielded === nothing ? f() : ScopedValues.with(f, CANCEL_TOKEN => something(shielded))
    # Exception unwind sigatomic automatically
    sigatomic_begin()
    res
end

"""
    exit_on_sigint(on::Bool)

Set `exit_on_sigint` flag of the julia runtime.  If `false`, Ctrl-C
(SIGINT) cancels the current ^C episode's cancellation scope and is
observed at cancellation points as a [`Base.CancellationRequest`](@ref),
which is capturable in a `try` block.
This is the default behavior in REPL, any code run via `-e` and `-E`
and in Julia script run with `-i` option.

If `true`, Ctrl-C terminates the process directly.  Running code
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
    if convention isa Tuple
        base_cconv = (convention..., gc_safe)
    else
        base_cconv = (convention, UInt16(0), gc_safe)
    end
    if !have_cancel
        cconv = Expr(:cconv, base_cconv, nreq)
        return Expr(:call, :ccall, esc(func), cconv, esc(rettype),
                     Expr(:tuple, map!(esc, types, types)...), map!(esc, args, args)...)
    end
    # Our internal ABI for cancellation handlers is f(func, state, args...). Codegen
    # inlines the establishment of the handler and the ultimate call to `f` has the
    # ordinary ABI without the extra arguments.
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

# Extended help

## Cancellation handlers

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

Both `CANCEL_FPTR` and `ref` are handled as ordinary ccall arguments (both
of C type `Ptr{Cvoid}`), including for purposes of rooting. Note that since
`CANCEL_FPTR` runs as a signal handler, it must be async-signal-safe in the
ordinary sense; no requirements are imposed on the called function itself.

The foreign call is never abandoned by the runtime, and no cancellation
point is implied by the `cancel_handler` option. It is the responsibility
of the user to place an appropriate cancellation point either before or
after (possibly conditional on an appropriate error return code) the ccall.

!!! compat "Julia 1.14"
    The `cancel_handler` option requires Julia 1.14 or higher.
"""
macro ccall(exprs...)
    return ccall_macro_lower((:ccall), ccall_macro_parse(exprs)...)
end

macro ccall_effects(effects::UInt16, exprs...)
    return ccall_macro_lower((:ccall, effects), ccall_macro_parse(exprs)...)
end
