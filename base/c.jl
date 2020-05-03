# This file is a part of Julia. License is MIT: https://julialang.org/license

# definitions related to C interface

import Core.Intrinsics: cglobal, bitcast

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
@static if ccall(:jl_get_UNAME, Any, ()) === :NT
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

"""
    Cwstring

A C-style string composed of the native wide character type
[`Cwchar_t`](@ref)s. `Cwstring`s are NUL-terminated. For
C-style strings composed of the native character
type, see [`Cstring`](@ref). For more information
about string interopability with C, see the
[manual](@ref man-bits-types).

"""
Cwstring

"""
    Cstring

A C-style string composed of the native character type
[`Cchar`](@ref)s. `Cstring`s are NUL-terminated. For
C-style strings composed of the native wide character
type, see [`Cwstring`](@ref). For more information
about string interopability with C, see the
[manual](@ref man-bits-types).
"""
Cstring

@static if ccall(:jl_get_UNAME, Any, ()) !== :NT
    const sizeof_mode_t = ccall(:jl_sizeof_mode_t, Cint, ())
    if sizeof_mode_t == 2
        const Cmode_t = Int16
    elseif sizeof_mode_t == 4
        const Cmode_t = Int32
    elseif sizeof_mode_t == 8
        const Cmode_t = Int64
    end
end

# construction from pointers
Cstring(p::Union{Ptr{Int8},Ptr{UInt8},Ptr{Cvoid}}) = bitcast(Cstring, p)
Cwstring(p::Union{Ptr{Cwchar_t},Ptr{Cvoid}})       = bitcast(Cwstring, p)
(::Type{Ptr{T}})(p::Cstring) where {T<:Union{Int8,UInt8,Cvoid}} = bitcast(Ptr{T}, p)
(::Type{Ptr{T}})(p::Cwstring) where {T<:Union{Cwchar_t,Cvoid}}  = bitcast(Ptr{Cwchar_t}, p)

convert(::Type{Cstring}, p::Union{Ptr{Int8},Ptr{UInt8},Ptr{Cvoid}}) = Cstring(p)
convert(::Type{Cwstring}, p::Union{Ptr{Cwchar_t},Ptr{Cvoid}}) = Cwstring(p)
convert(::Type{Ptr{T}}, p::Cstring) where {T<:Union{Int8,UInt8,Cvoid}} = Ptr{T}(p)
convert(::Type{Ptr{T}}, p::Cwstring) where {T<:Union{Cwchar_t,Cvoid}} = Ptr{T}(p)

"""
    pointer(array [, index])

Get the native address of an array or string, optionally at a given location `index`.

This function is "unsafe". Be careful to ensure that a Julia reference to
`array` exists as long as this pointer will be used. The [`GC.@preserve`](@ref)
macro should be used to protect the `array` argument from garbage collection
within a given block of code.

Calling [`Ref(array[, index])`](@ref Ref) is generally preferable to this function as it guarantees validity.
"""
function pointer end

pointer(p::Cstring) = convert(Ptr{Cchar}, p)
pointer(p::Cwstring) = convert(Ptr{Cwchar_t}, p)

# comparisons against pointers (mainly to support `cstr==C_NULL`)
==(x::Union{Cstring,Cwstring}, y::Ptr) = pointer(x) == y
==(x::Ptr, y::Union{Cstring,Cwstring}) = x == pointer(y)

unsafe_string(s::Cstring) = unsafe_string(convert(Ptr{UInt8}, s))

# convert strings to String etc. to pass as pointers
cconvert(::Type{Cstring}, s::String) = s
cconvert(::Type{Cstring}, s::AbstractString) =
    cconvert(Cstring, String(s)::String)

function cconvert(::Type{Cwstring}, s::AbstractString)
    v = transcode(Cwchar_t, String(s))
    !isempty(v) && v[end] == 0 || push!(v, 0)
    return v
end

eltype(::Type{Cstring}) = Cchar
eltype(::Type{Cwstring}) = Cwchar_t

containsnul(p::Ptr, len) =
    C_NULL != ccall(:memchr, Ptr{Cchar}, (Ptr{Cchar}, Cint, Csize_t), p, 0, len)
containsnul(s::String) = containsnul(unsafe_convert(Ptr{Cchar}, s), sizeof(s))
containsnul(s::AbstractString) = '\0' in s

function unsafe_convert(::Type{Cstring}, s::Union{String,AbstractVector{UInt8}})
    p = unsafe_convert(Ptr{Cchar}, s)
    containsnul(p, sizeof(s)) &&
        throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    return Cstring(p)
end

function unsafe_convert(::Type{Cwstring}, v::Vector{Cwchar_t})
    for i = 1:length(v)-1
        v[i] == 0 &&
            throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(v))"))
    end
    v[end] == 0 ||
        throw(ArgumentError("C string data must be NUL terminated: $(repr(v))"))
    p = unsafe_convert(Ptr{Cwchar_t}, v)
    return Cwstring(p)
end

# symbols are guaranteed not to contain embedded NUL
cconvert(::Type{Cstring}, s::Symbol) = s
unsafe_convert(::Type{Cstring}, s::Symbol) = Cstring(unsafe_convert(Ptr{Cchar}, s))

@static if ccall(:jl_get_UNAME, Any, ()) === :NT
"""
    Base.cwstring(s)

Converts a string `s` to a NUL-terminated `Vector{Cwchar_t}`, suitable for passing to C
functions expecting a `Ptr{Cwchar_t}`. The main advantage of using this over the implicit
conversion provided by [`Cwstring`](@ref) is if the function is called multiple times with the
same argument.

This is only available on Windows.
"""
function cwstring(s::AbstractString)
    bytes = codeunits(String(s))
    0 in bytes && throw(ArgumentError("embedded NULs are not allowed in C strings: $(repr(s))"))
    return push!(transcode(UInt16, bytes), 0)
end
end

# transcoding between data in UTF-8 and UTF-16 for Windows APIs,
# and also UTF-32 for APIs using Cwchar_t on other platforms.

"""
    transcode(T, src)

Convert string data between Unicode encodings. `src` is either a
`String` or a `Vector{UIntXX}` of UTF-XX code units, where
`XX` is 8, 16, or 32. `T` indicates the encoding of the return value:
`String` to return a (UTF-8 encoded) `String` or `UIntXX`
to return a `Vector{UIntXX}` of UTF-`XX` data. (The alias [`Cwchar_t`](@ref)
can also be used as the integer type, for converting `wchar_t*` strings
used by external C libraries.)

The `transcode` function succeeds as long as the input data can be
reasonably represented in the target encoding; it always succeeds for
conversions between UTF-XX encodings, even for invalid Unicode data.

Only conversion to/from UTF-8 is currently supported.
"""
function transcode end

transcode(::Type{T}, src::AbstractVector{T}) where {T<:Union{UInt8,UInt16,UInt32,Int32}} = src
transcode(::Type{T}, src::String) where {T<:Union{Int32,UInt32}} = T[T(c) for c in src]
transcode(::Type{T}, src::AbstractVector{UInt8}) where {T<:Union{Int32,UInt32}} =
    transcode(T, String(Vector(src)))
transcode(::Type{T}, src::CodeUnits{UInt8,String}) where {T<:Union{Int32,UInt32}} =
    transcode(T, String(src))

function transcode(::Type{UInt8}, src::Vector{<:Union{Int32,UInt32}})
    buf = IOBuffer()
    for c in src
        print(buf, Char(c))
    end
    take!(buf)
end
transcode(::Type{String}, src::String) = src
transcode(T, src::String) = transcode(T, codeunits(src))
transcode(::Type{String}, src) = String(transcode(UInt8, src))

function transcode(::Type{UInt16}, src::AbstractVector{UInt8})
    require_one_based_indexing(src)
    dst = UInt16[]
    i, n = 1, length(src)
    n > 0 || return dst
    sizehint!(dst, 2n)
    a = src[1]
    while true
        if i < n && -64 <= a % Int8 <= -12 # multi-byte character
            b = src[i += 1]
            if -64 <= (b % Int8) || a == 0xf4 && 0x8f < b
                # invalid UTF-8 (non-continuation or too-high code point)
                push!(dst, a)
                a = b; continue
            elseif a < 0xe0 # 2-byte UTF-8
                push!(dst, xor(0x3080, UInt16(a) << 6, b))
            elseif i < n # 3/4-byte character
                c = src[i += 1]
                if -64 <= (c % Int8) # invalid UTF-8 (non-continuation)
                    push!(dst, a, b)
                    a = c; continue
                elseif a < 0xf0 # 3-byte UTF-8
                    push!(dst, xor(0x2080, UInt16(a) << 12, UInt16(b) << 6, c))
                elseif i < n
                    d = src[i += 1]
                    if -64 <= (d % Int8) # invalid UTF-8 (non-continuation)
                        push!(dst, a, b, c)
                        a = d; continue
                    elseif a == 0xf0 && b < 0x90 # overlong encoding
                        push!(dst, xor(0x2080, UInt16(b) << 12, UInt16(c) << 6, d))
                    else # 4-byte UTF-8
                        push!(dst, 0xe5b8 + (UInt16(a) << 8) + (UInt16(b) << 2) + (c >> 4),
                                   xor(0xdc80, UInt16(c & 0xf) << 6, d))
                    end
                else # too short
                    push!(dst, a, b, c)
                    break
                end
            else # too short
                push!(dst, a, b)
                break
            end
        else # ASCII or invalid UTF-8 (continuation byte or too-high code point)
            push!(dst, a)
        end
        i < n || break
        a = src[i += 1]
    end
    return dst
end

function transcode(::Type{UInt8}, src::AbstractVector{UInt16})
    require_one_based_indexing(src)
    n = length(src)
    n == 0 && return UInt8[]

    # Precompute m = sizeof(dst).   This involves annoying duplication
    # of the loop over the src array.   However, this is not just an
    # optimization: it is problematic for security reasons to grow
    # dst dynamically, because Base.winprompt uses this function to
    # convert passwords to UTF-8 and we don't want to make unintentional
    # copies of the password data.
    a = src[1]
    i, m = 1, 0
    while true
        if a < 0x80
            m += 1
        elseif a < 0x800 # 2-byte UTF-8
            m += 2
        elseif a & 0xfc00 == 0xd800 && i < length(src)
            b = src[i += 1]
            if (b & 0xfc00) == 0xdc00 # 2-unit UTF-16 sequence => 4-byte UTF-8
                m += 4
            else
                m += 3
                a = b; continue
            end
        else
            # 1-unit high UTF-16 or unpaired high surrogate
            # either way, encode as 3-byte UTF-8 code point
            m += 3
        end
        i < n || break
        a = src[i += 1]
    end

    dst = StringVector(m)
    a = src[1]
    i, j = 1, 0
    while true
        if a < 0x80 # ASCII
            dst[j += 1] = a % UInt8
        elseif a < 0x800 # 2-byte UTF-8
            dst[j += 1] = 0xc0 | ((a >> 6) % UInt8)
            dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
        elseif a & 0xfc00 == 0xd800 && i < n
            b = src[i += 1]
            if (b & 0xfc00) == 0xdc00
                # 2-unit UTF-16 sequence => 4-byte UTF-8
                a += 0x2840
                dst[j += 1] = 0xf0 | ((a >> 8) % UInt8)
                dst[j += 1] = 0x80 | ((a % UInt8) >> 2)
                dst[j += 1] = xor(0xf0, ((a % UInt8) << 4) & 0x3f, (b >> 6) % UInt8)
                dst[j += 1] = 0x80 | ((b % UInt8) & 0x3f)
            else
                dst[j += 1] = 0xe0 | ((a >> 12) % UInt8)
                dst[j += 1] = 0x80 | (((a >> 6) % UInt8) & 0x3f)
                dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
                a = b; continue
            end
        else
            # 1-unit high UTF-16 or unpaired high surrogate
            # either way, encode as 3-byte UTF-8 code point
            dst[j += 1] = 0xe0 | ((a >> 12) % UInt8)
            dst[j += 1] = 0x80 | (((a >> 6) % UInt8) & 0x3f)
            dst[j += 1] = 0x80 | ((a % UInt8) & 0x3f)
        end
        i < n || break
        a = src[i += 1]
    end
    return dst
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

function ccallable(f::Function, rt::Type, argt::Type, name::Union{AbstractString,Symbol}=string(f))
    ccall(:jl_extern_c, Cvoid, (Any, Any, Any, Cstring), f, rt, argt, name)
end

function expand_ccallable(rt, def)
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
            name = sig.args[1]
            at = map(sig.args[2:end]) do a
                if isa(a,Expr) && a.head === :(::)
                    a.args[2]
                else
                    :Any
                end
            end
            return quote
                $(esc(def))
                ccallable($(esc(name)), $(esc(rt)), $(Expr(:curly, :Tuple, map(esc, at)...)), $(string(name)))
            end
        end
    end
    error("expected method definition in @ccallable")
end

macro ccallable(def)
    expand_ccallable(nothing, def)
end
macro ccallable(rt, def)
    expand_ccallable(rt, def)
end

# @ccall implementation
"""
    ccall_macro_parse(expression)

`ccall_macro_parse` is an implementation detail of `@ccall

it takes an expression like `:(printf("%d"::Cstring, value::Cuint)::Cvoid)`
returns: a tuple of `(function_name, return_type, arg_types, args)`

The above input outputs this:

    (:printf, :Cvoid, [:Cstring, :Cuint], ["%d", :value])
"""
function ccall_macro_parse(expr::Expr)
    # setup and check for errors
    if !Meta.isexpr(expr, :(::))
        throw(ArgumentError("@ccall needs a function signature with a return type"))
    end
    rettype = expr.args[2]

    call = expr.args[1]
    if !Meta.isexpr(call, :call)
        throw(ArgumentError("@ccall has to take a function call"))
    end

    # get the function symbols
    func = let f = call.args[1]
        if Meta.isexpr(f, :.)
            :(($(f.args[2]), $(f.args[1])))
        elseif Meta.isexpr(f, :$)
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
    if length(callargs) >= 2 && Meta.isexpr(callargs[2], :parameters)
        argstart = 3
        varargs = callargs[2].args
    end

    # collect args and types
    args = []
    types = []

    function pusharg!(arg)
        if !Meta.isexpr(arg, :(::))
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

    return func, rettype, types, args, nreq
end


function ccall_macro_lower(convention, func, rettype, types, args, nreq)
    lowering = []
    realargs = []
    gcroots = []

    # if interpolation was used, ensure  variable is a function pointer at runtime.
    if Meta.isexpr(func, :$)
        push!(lowering, Expr(:(=), :func, esc(func.args[1])))
        name = QuoteNode(func.args[1])
        func = :func
        check = quote
            if !isa(func, Ptr{Cvoid})
                name = $name
                throw(ArgumentError("interpolated function `$name` was not a Ptr{Cvoid}, but $(typeof(func))"))
            end
        end
        push!(lowering, check)
    else
        func = esc(func)
    end

    for (i, (arg, type)) in enumerate(zip(args, types))
        sym = Symbol(string("arg", i, "root"))
        sym2 = Symbol(string("arg", i, ))
        earg, etype = esc(arg), esc(type)
        push!(lowering, :($sym = Base.cconvert($etype, $earg)))
        push!(lowering, :($sym2 = Base.unsafe_convert($etype, $sym)))
        push!(realargs, sym2)
        push!(gcroots, sym)
    end
    etypes = Expr(:call, Expr(:core, :svec), types...)
    exp = Expr(:foreigncall,
               func,
               esc(rettype),
               esc(etypes),
               nreq,
               QuoteNode(convention),
               realargs..., gcroots...)
    push!(lowering, exp)

    return Expr(:block, lowering...)
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

    @ccall sprintf("%s = %d"::Cstring ; "foo"::Cstring, foo::Cint)::Cint

The semicolon is used to separate required arguments (of which there
must be at least one) from variadic arguments.

Example using an external library:

    # C signature of g_uri_escape_string:
    # char *g_uri_escape_string(const char *unescaped, const char *reserved_chars_allowed, gboolean allow_utf8);

    const glib = "libglib-2.0"
    @ccall glib.g_uri_escape_string(my_uri::Cstring, ":/"::Cstring, true::Cint)::Cstring

The string literal could also be used directly before the function
name, if desired `"libglib-2.0".g_uri_escape_string(...`
"""
macro ccall(expr)
    return ccall_macro_lower(:ccall, ccall_macro_parse(expr)...)
end
