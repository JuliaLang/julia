# This file is a part of Julia. License is MIT: https://julialang.org/license

# definitions related to C interface

import Core.Intrinsics: cglobal

"""
    cglobal((symbol, library) [, type=Cvoid])

Obtain a pointer to a global variable in a C-exported shared library, specified exactly as
in [`ccall`](@ref).
Returns a `Ptr{Type}`, defaulting to `Ptr{Cvoid}` if no `Type` argument is
supplied.
The values can be read or written by [`unsafe_load`](@ref) or [`unsafe_store!`](@ref),
respectively.
"""
cglobal

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
            at = map(sig.args[2:end]) do a
                if isa(a,Expr) && a.head === :(::)
                    a.args[end]
                else
                    :Any
                end
            end
            return quote
                @__doc__ $(esc(def))
                _ccallable($name, $(esc(rt)), $(Expr(:curly, :Tuple, esc(f), map(esc, at)...)))
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
    expr = nothing
    if exprs isa Expr
        expr = exprs
    elseif length(exprs) == 1
        expr = exprs[1]
    elseif length(exprs) == 2
        gc_expr = exprs[1]
        expr = exprs[2]
        if gc_expr.head == :(=) && gc_expr.args[1] == :gc_safe
            if gc_expr.args[2] == true
                gc_safe = true
            elseif gc_expr.args[2] == false
                gc_safe = false
            else
                throw(ArgumentError("gc_safe must be true or false"))
            end
        else
            throw(ArgumentError("@ccall option must be `gc_safe=true` or `gc_safe=false`"))
        end
    else
        throw(ArgumentError("@ccall needs a function signature with a return type"))
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
            :(($(f.args[2]), $(f.args[1])))
        elseif isexpr(f, :$)
            f
        elseif f isa Symbol
            QuoteNode(f)
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
    if !isnothing(varargs)
        if length(args) == 0
            throw(ArgumentError("C ABI prohibits vararg without one required argument"))
        end
        nreq = length(args)
        for a in varargs
            pusharg!(a)
        end
    end
    return func, rettype, types, args, gc_safe, nreq
end


function ccall_macro_lower(convention, func, rettype, types, args, gc_safe, nreq)
    statements = []

    # if interpolation was used, ensure the value is a function pointer at runtime.
    if isexpr(func, :$)
        push!(statements, Expr(:(=), :func, esc(func.args[1])))
        name = QuoteNode(func.args[1])
        func = :func
        check = quote
            if !isa(func, Ptr{Cvoid})
                name = $name
                throw(ArgumentError(LazyString("interpolated function `", name, "` was not a Ptr{Cvoid}, but ", typeof(func))))
            end
        end
        push!(statements, check)
    else
        func = esc(func)
    end
    cconv = nothing
    if convention isa Tuple
        cconv = Expr(:cconv, (convention..., gc_safe), nreq)
    else
        cconv = Expr(:cconv, (convention, UInt16(0), gc_safe), nreq)
    end

    return Expr(:block, statements...,
                Expr(:call, :ccall, func, cconv, esc(rettype),
                     Expr(:tuple, map(esc, types)...), map(esc, args)...))
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
WARNING: This option should be used with caution, as it can lead to undefined behavior if the ccall
calls back into the julia runtime. (`@cfunction`/`@ccallables` are safe however)
"""
macro ccall(exprs...)
    return ccall_macro_lower((:ccall), ccall_macro_parse(exprs)...)
end

macro ccall_effects(effects::UInt16, expr)
    return ccall_macro_lower((:ccall, effects), ccall_macro_parse(expr)...)
end
