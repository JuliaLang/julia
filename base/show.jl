# This file is a part of Julia. License is MIT: https://julialang.org/license

show(io::IO, ::UndefInitializer) = print(io, "array initializer with undefined values")

# first a few multiline show functions for types defined before the MIME type:

show(io::IO, ::MIME"text/plain", r::AbstractRange) = show(io, r) # always use the compact form for printing ranges

function show(io::IO, ::MIME"text/plain", r::LinRange)
    # show for LinRange, e.g.
    # range(1, stop=3, length=7)
    # 7-element LinRange{Float64}:
    #   1.0,1.33333,1.66667,2.0,2.33333,2.66667,3.0
    print(io, summary(r))
    if !isempty(r)
        println(io, ":")
        print_range(io, r)
    end
end

function show(io::IO, ::MIME"text/plain", f::Function)
    ft = typeof(f)
    mt = ft.name.mt
    if isa(f, Core.IntrinsicFunction)
        show(io, f)
        id = Core.Intrinsics.bitcast(Int32, f)
        print(io, " (intrinsic function #$id)")
    elseif isa(f, Core.Builtin)
        print(io, mt.name, " (built-in function)")
    else
        name = mt.name
        isself = isdefined(ft.name.module, name) &&
                 ft == typeof(getfield(ft.name.module, name))
        n = length(methods(f))
        m = n==1 ? "method" : "methods"
        sname = string(name)
        ns = (isself || '#' in sname) ? sname : string("(::", ft, ")")
        what = startswith(ns, '@') ? "macro" : "generic function"
        print(io, ns, " (", what, " with $n $m)")
    end
end

function show(io::IO, ::MIME"text/plain", iter::Union{KeySet,ValueIterator})
    print(io, summary(iter))
    isempty(iter) && return
    print(io, ". ", isa(iter,KeySet) ? "Keys" : "Values", ":")
    limit::Bool = get(io, :limit, false)
    if limit
        sz = displaysize(io)
        rows, cols = sz[1] - 3, sz[2]
        rows < 2 && (print(io, " …"); return)
        cols < 4 && (cols = 4)
        cols -= 2 # For prefix "  "
        rows -= 1 # For summary
    else
        rows = cols = typemax(Int)
    end

    for (i, v) in enumerate(iter)
        print(io, "\n  ")
        i == rows < length(iter) && (print(io, "⋮"); break)

        if limit
            str = sprint(show, v, context=io, sizehint=0)
            str = _truncate_at_width_or_chars(str, cols, "\r\n")
            print(io, str)
        else
            show(io, v)
        end
    end
end

function show(io::IO, ::MIME"text/plain", t::AbstractDict{K,V}) where {K,V}
    # show more descriptively, with one line per key/value pair
    recur_io = IOContext(io, :SHOWN_SET => t)
    limit::Bool = get(io, :limit, false)
    if !haskey(io, :compact)
        recur_io = IOContext(recur_io, :compact => true)
    end

    print(io, summary(t))
    isempty(t) && return
    print(io, ":")
    show_circular(io, t) && return
    if limit
        sz = displaysize(io)
        rows, cols = sz[1] - 3, sz[2]
        rows < 2   && (print(io, " …"); return)
        cols < 12  && (cols = 12) # Minimum widths of 2 for key, 4 for value
        cols -= 6 # Subtract the widths of prefix "  " separator " => "
        rows -= 1 # Subtract the summary

        # determine max key width to align the output, caching the strings
        ks = Vector{AbstractString}(undef, min(rows, length(t)))
        vs = Vector{AbstractString}(undef, min(rows, length(t)))
        keylen = 0
        vallen = 0
        for (i, (k, v)) in enumerate(t)
            i > rows && break
            ks[i] = sprint(show, k, context=recur_io, sizehint=0)
            vs[i] = sprint(show, v, context=recur_io, sizehint=0)
            keylen = clamp(length(ks[i]), keylen, cols)
            vallen = clamp(length(vs[i]), vallen, cols)
        end
        if keylen > max(div(cols, 2), cols - vallen)
            keylen = max(cld(cols, 3), cols - vallen)
        end
    else
        rows = cols = typemax(Int)
    end

    for (i, (k, v)) in enumerate(t)
        print(io, "\n  ")
        i == rows < length(t) && (print(io, rpad("⋮", keylen), " => ⋮"); break)

        if limit
            key = rpad(_truncate_at_width_or_chars(ks[i], keylen, "\r\n"), keylen)
        else
            key = sprint(show, k, context=recur_io, sizehint=0)
        end
        print(recur_io, key)
        print(io, " => ")

        if limit
            val = _truncate_at_width_or_chars(vs[i], cols - keylen, "\r\n")
            print(io, val)
        else
            show(recur_io, v)
        end
    end
end

function show(io::IO, ::MIME"text/plain", opt::JLOptions)
    println(io, "JLOptions(")
    fields = fieldnames(JLOptions)
    nfields = length(fields)
    for (i, f) in enumerate(fields)
        v = getfield(opt, i)
        if isa(v, Ptr{UInt8})
            v = (v != C_NULL) ? unsafe_string(v) : ""
        elseif isa(v, Ptr{Ptr{UInt8}})
            v = unsafe_load_commands(v)
        end
        println(io, "  ", f, " = ", repr(v), i < nfields ? "," : "")
    end
    print(io, ")")
end

function show(io::IO, ::MIME"text/plain", t::Task)
    show(io, t)
    if t.state == :failed
        println(io)
        showerror(io, CapturedException(t.result, t.backtrace))
    end
end


print(io::IO, s::Symbol) = (write(io,s); nothing)

"""
    IOContext

`IOContext` provides a mechanism for passing output configuration settings among [`show`](@ref) methods.

In short, it is an immutable dictionary that is a subclass of `IO`. It supports standard
dictionary operations such as [`getindex`](@ref), and can also be used as an I/O stream.
"""
struct IOContext{IO_t <: IO} <: AbstractPipe
    io::IO_t
    dict::ImmutableDict{Symbol, Any}

    function IOContext{IO_t}(io::IO_t, dict::ImmutableDict{Symbol, Any}) where IO_t<:IO
        @assert !(IO_t <: IOContext) "Cannot create `IOContext` from another `IOContext`."
        return new(io, dict)
    end
end

# (Note that TTY and TTYTerminal io types have a :color property.)
unwrapcontext(io::IO) = io, get(io,:color,false) ? ImmutableDict{Symbol,Any}(:color, true) : ImmutableDict{Symbol,Any}()
unwrapcontext(io::IOContext) = io.io, io.dict

function IOContext(io::IO, dict::ImmutableDict)
    io0 = unwrapcontext(io)[1]
    IOContext{typeof(io0)}(io0, dict)
end

convert(::Type{IOContext}, io::IO) = IOContext(unwrapcontext(io)...)

IOContext(io::IO) = convert(IOContext, io)

function IOContext(io::IO, KV::Pair)
    io0, d = unwrapcontext(io)
    IOContext(io0, ImmutableDict{Symbol,Any}(d, KV[1], KV[2]))
end

"""
    IOContext(io::IO, context::IOContext)

Create an `IOContext` that wraps an alternate `IO` but inherits the properties of `context`.
"""
IOContext(io::IO, context::IO) = IOContext(unwrapcontext(io)[1], unwrapcontext(context)[2])

"""
    IOContext(io::IO, KV::Pair...)

Create an `IOContext` that wraps a given stream, adding the specified `key=>value` pairs to
the properties of that stream (note that `io` can itself be an `IOContext`).

 - use `(key => value) in io` to see if this particular combination is in the properties set
 - use `get(io, key, default)` to retrieve the most recent value for a particular key

The following properties are in common use:

 - `:compact`: Boolean specifying that small values should be printed more compactly, e.g.
   that numbers should be printed with fewer digits. This is set when printing array
   elements.
 - `:limit`: Boolean specifying that containers should be truncated, e.g. showing `…` in
   place of most elements.
 - `:displaysize`: A `Tuple{Int,Int}` giving the size in rows and columns to use for text
   output. This can be used to override the display size for called functions, but to
   get the size of the screen use the `displaysize` function.
 - `:typeinfo`: a `Type` characterizing the information already printed
   concerning the type of the object about to be displayed. This is mainly useful when
   displaying a collection of objects of the same type, so that redundant type information
   can be avoided (e.g. `[Float16(0)]` can be shown as "Float16[0.0]" instead
   of "Float16[Float16(0.0)]" : while displaying the elements of the array, the `:typeinfo`
   property will be set to `Float16`).
 - `:color`: Boolean specifying whether ANSI color/escape codes are supported/expected.
   By default, this is determined by whether `io` is a compatible terminal and by any
   `--color` command-line flag when `julia` was launched.

# Examples

```jldoctest
julia> io = IOBuffer();

julia> printstyled(IOContext(io, :color => true), "string", color=:red)

julia> String(take!(io))
"\\e[31mstring\\e[39m"

julia> printstyled(io, "string", color=:red)

julia> String(take!(io))
"string"
```

```jldoctest
julia> print(IOContext(stdout, :compact => false), 1.12341234)
1.12341234
julia> print(IOContext(stdout, :compact => true), 1.12341234)
1.12341
```

```jldoctest
julia> function f(io::IO)
           if get(io, :short, false)
               print(io, "short")
           else
               print(io, "loooooong")
           end
       end
f (generic function with 1 method)

julia> f(stdout)
loooooong
julia> f(IOContext(stdout, :short => true))
short
```
"""
IOContext(io::IO, KV::Pair, KVs::Pair...) = IOContext(IOContext(io, KV), KVs...)

show(io::IO, ctx::IOContext) = (print(io, "IOContext("); show(io, ctx.io); print(io, ")"))

pipe_reader(io::IOContext) = io.io
pipe_writer(io::IOContext) = io.io
lock(io::IOContext) = lock(io.io)
unlock(io::IOContext) = unlock(io.io)

in(key_value::Pair, io::IOContext) = in(key_value, io.dict, ===)
in(key_value::Pair, io::IO) = false
haskey(io::IOContext, key) = haskey(io.dict, key)
haskey(io::IO, key) = false
getindex(io::IOContext, key) = getindex(io.dict, key)
getindex(io::IO, key) = throw(KeyError(key))
get(io::IOContext, key, default) = get(io.dict, key, default)
get(io::IO, key, default) = default

displaysize(io::IOContext) = haskey(io, :displaysize) ? io[:displaysize] : displaysize(io.io)

show_circular(io::IO, @nospecialize(x)) = false
function show_circular(io::IOContext, @nospecialize(x))
    d = 1
    for (k, v) in io.dict
        if k === :SHOWN_SET
            if v === x
                print(io, "#= circular reference @-$d =#")
                return true
            end
            d += 1
        end
    end
    return false
end

"""
    show(x)

Write an informative text representation of a value to the current output stream. New types
should overload `show(io::IO, x)` where the first argument is a stream. The representation used
by `show` generally includes Julia-specific formatting and type information.

[`repr`](@ref) returns the output of `show` as a string.

See also [`print`](@ref), which writes un-decorated representations.

# Examples
```jldoctest
julia> show("Hello World!")
"Hello World!"
julia> print("Hello World!")
Hello World!
```
"""
show(x) = show(stdout::IO, x)

show(io::IO, @nospecialize(x)) = show_default(io, x)

function show_default(io::IO, @nospecialize(x))
    t = typeof(x)::DataType
    show(io, t)
    print(io, '(')
    nf = nfields(x)
    nb = sizeof(x)
    if nf != 0 || nb == 0
        if !show_circular(io, x)
            recur_io = IOContext(io, Pair{Symbol,Any}(:SHOWN_SET, x),
                                 Pair{Symbol,Any}(:typeinfo, Any))
            for i in 1:nf
                f = fieldname(t, i)
                if !isdefined(x, f)
                    print(io, undef_ref_str)
                else
                    show(recur_io, getfield(x, i))
                end
                if i < nf
                    print(io, ", ")
                end
            end
        end
    else
        print(io, "0x")
        r = Ref(x)
        GC.@preserve r begin
            p = unsafe_convert(Ptr{Cvoid}, r)
            for i in (nb - 1):-1:0
                print(io, string(unsafe_load(convert(Ptr{UInt8}, p + i)), base = 16, pad = 2))
            end
        end
    end
    print(io,')')
end

# Check if a particular symbol is exported from a standard library module
function is_exported_from_stdlib(name::Symbol, mod::Module)
    !isdefined(mod, name) && return false
    orig = getfield(mod, name)
    while !(mod === Base || mod === Core)
        parent = parentmodule(mod)
        if mod === Main || mod === parent || parent === Main
            return false
        end
        mod = parent
    end
    return isexported(mod, name) && isdefined(mod, name) && !isdeprecated(mod, name) && getfield(mod, name) === orig
end

function show(io::IO, f::Function)
    ft = typeof(f)
    mt = ft.name.mt
    if isdefined(mt, :module) && isdefined(mt.module, mt.name) &&
        getfield(mt.module, mt.name) === f
        if is_exported_from_stdlib(mt.name, mt.module) || mt.module === Main || get(io, :compact, false)
            print(io, mt.name)
        else
            print(io, mt.module, ".", mt.name)
        end
    else
        show_default(io, f)
    end
end

function show(io::IO, x::Core.IntrinsicFunction)
    name = ccall(:jl_intrinsic_name, Cstring, (Core.IntrinsicFunction,), x)
    print(io, unsafe_string(name))
end

show(io::IO, ::Core.TypeofBottom) = print(io, "Union{}")

function show(io::IO, x::Union)
    print(io, "Union")
    show_comma_array(io, uniontypes(x), '{', '}')
end

function print_without_params(@nospecialize(x))
    if isa(x,UnionAll)
        b = unwrap_unionall(x)
        return isa(b,DataType) && b.name.wrapper === x
    end
    return false
end

has_typevar(@nospecialize(t), v::TypeVar) = ccall(:jl_has_typevar, Cint, (Any, Any), t, v)!=0

function io_has_tvar_name(io::IOContext, name::Symbol, @nospecialize(x))
    for (key, val) in io.dict
        if key === :unionall_env && val isa TypeVar && val.name === name && has_typevar(x, val)
            return true
        end
    end
    return false
end
io_has_tvar_name(io::IO, name::Symbol, @nospecialize(x)) = false

function show(io::IO, x::UnionAll)
    if print_without_params(x)
        return show(io, unwrap_unionall(x).name)
    end

    if x.var.name == :_ || io_has_tvar_name(io, x.var.name, x)
        counter = 1
        while true
            newname = Symbol(x.var.name, counter)
            if !io_has_tvar_name(io, newname, x)
                newtv = TypeVar(newname, x.var.lb, x.var.ub)
                x = UnionAll(newtv, x{newtv})
                break
            end
            counter += 1
        end
    end

    show(IOContext(io, :unionall_env => x.var), x.body)
    print(io, " where ")
    show(io, x.var)
end

show(io::IO, x::DataType) = show_datatype(io, x)

# Check whether 'sym' (defined in module 'parent') is visible from module 'from'
# If an object with this name exists in 'from', we need to check that it's the same binding
# and that it's not deprecated.
function isvisible(sym::Symbol, parent::Module, from::Module)
    owner = ccall(:jl_binding_owner, Any, (Any, Any), parent, sym)
    from_owner = ccall(:jl_binding_owner, Any, (Any, Any), from, sym)
    return owner !== nothing && from_owner === owner &&
        !isdeprecated(parent, sym) &&
        isdefined(from, sym) # if we're going to return true, force binding resolution
end

function show_type_name(io::IO, tn::Core.TypeName)
    if tn === UnionAll.name
        # by coincidence, `typeof(Type)` is a valid representation of the UnionAll type.
        # intercept this case and print `UnionAll` instead.
        return print(io, "UnionAll")
    end
    globname = isdefined(tn, :mt) ? tn.mt.name : nothing
    globfunc = false
    if globname !== nothing
        globname_str = string(globname)
        if ('#' ∉ globname_str && '@' ∉ globname_str && isdefined(tn, :module) &&
                isbindingresolved(tn.module, globname) && isdefined(tn.module, globname) &&
                isconcretetype(tn.wrapper) && isa(getfield(tn.module, globname), tn.wrapper))
            globfunc = true
        end
    end
    sym = globfunc ? globname : tn.name
    if get(io, :compact, false)
        if globfunc
            return print(io, "typeof(", sym, ")")
        else
            return print(io, sym)
        end
    end
    sym_str = string(sym)
    hidden = !globfunc && '#' ∈ sym_str
    quo = false
    if hidden
        print(io, "getfield(")
    elseif globfunc
        print(io, "typeof(")
    end
    # Print module prefix unless type is visible from module passed to IOContext
    # If :module is not set, default to Main. nothing can be used to force printing prefix
    from = get(io, :module, Main)
    if isdefined(tn, :module) && (hidden || from === nothing || !isvisible(sym, tn.module, from))
        show(io, tn.module)
        if !hidden
            print(io, ".")
            if globfunc && !is_id_start_char(first(sym_str))
                print(io, ":")
                if sym == :(==)
                    print(io, "(")
                    quo = true
                end
            end
        end
    end
    if hidden
        print(io, ", Symbol(\"", sym_str, "\"))")
    else
        print(io, sym_str)
        if globfunc
            print(io, ")")
            if quo
                print(io, ")")
            end
        end
    end
end

function show_datatype(io::IO, x::DataType)
    istuple = x.name === Tuple.name
    if (!isempty(x.parameters) || istuple) && x !== Tuple
        n = length(x.parameters)

        # Print homogeneous tuples with more than 3 elements compactly as NTuple{N, T}
        if istuple && n > 3 && all(i -> (x.parameters[1] === i), x.parameters)
            print(io, "NTuple{", n, ',', x.parameters[1], "}")
        else
            show_type_name(io, x.name)
            # Do not print the type parameters for the primary type if we are
            # printing a method signature or type parameter.
            # Always print the type parameter if we are printing the type directly
            # since this information is still useful.
            print(io, '{')
            for (i, p) in enumerate(x.parameters)
                show(io, p)
                i < n && print(io, ',')
            end
            print(io, '}')
        end
    else
        show_type_name(io, x.name)
    end
end

function show_supertypes(io::IO, typ::DataType)
    print(io, typ)
    while typ != Any
        typ = supertype(typ)
        print(io, " <: ", typ)
    end
end

show_supertypes(typ::DataType) = show_supertypes(stdout, typ)

"""
    @show

Show an expression and result, returning the result.
"""
macro show(exs...)
    blk = Expr(:block)
    for ex in exs
        push!(blk.args, :(println($(sprint(show_unquoted,ex)*" = "),
                                  repr(begin value=$(esc(ex)) end))))
    end
    isempty(exs) || push!(blk.args, :value)
    return blk
end

function show(io::IO, tn::Core.TypeName)
    show_type_name(io, tn)
end

show(io::IO, ::Nothing) = print(io, "nothing")
print(io::IO, ::Nothing) = throw(ArgumentError("`nothing` should not be printed; use `show`, `repr`, or custom output instead."))
show(io::IO, b::Bool) = print(io, b ? "true" : "false")
show(io::IO, n::Signed) = (write(io, string(n)); nothing)
show(io::IO, n::Unsigned) = print(io, "0x", string(n, pad = sizeof(n)<<1, base = 16))
print(io::IO, n::Unsigned) = print(io, string(n))

show(io::IO, p::Ptr) = print(io, typeof(p), " @0x$(string(UInt(p), base = 16, pad = Sys.WORD_SIZE>>2))")

has_tight_type(p::Pair) =
    typeof(p.first)  == typeof(p).parameters[1] &&
    typeof(p.second) == typeof(p).parameters[2]

isdelimited(io::IO, x) = true

# !isdelimited means that the Pair is printed with "=>" (like in "1 => 2"),
# without its explicit type (like in "Pair{Integer,Integer}(1, 2)")
isdelimited(io::IO, p::Pair) = !(has_tight_type(p) || get(io, :typeinfo, Any) == typeof(p))

function gettypeinfos(io::IO, p::Pair)
    typeinfo = get(io, :typeinfo, Any)
    p isa typeinfo <: Pair ?
        fieldtype(typeinfo, 1) => fieldtype(typeinfo, 2) :
        Any => Any
end

function show(io::IO, p::Pair)
    iocompact = IOContext(io, :compact => get(io, :compact, true))
    isdelimited(io, p) && return show_default(iocompact, p)
    typeinfos = gettypeinfos(io, p)
    for i = (1, 2)
        io_i = IOContext(iocompact, :typeinfo => typeinfos[i])
        isdelimited(io_i, p[i]) || print(io, "(")
        show(io_i, p[i])
        isdelimited(io_i, p[i]) || print(io, ")")
        i == 1 && print(io, get(io, :compact, false) ? "=>" : " => ")
    end
end

function show(io::IO, m::Module)
    if is_root_module(m)
        print(io, nameof(m))
    else
        print(io, join(fullname(m),"."))
    end
end

function sourceinfo_slotnames(src::CodeInfo)
    slotnames = src.slotnames
    isa(slotnames, Array) || return String[]
    names = Dict{String,Int}()
    printnames = Vector{String}(undef, length(slotnames))
    for i in eachindex(slotnames)
        name = string(slotnames[i])
        idx = get!(names, name, i)
        if idx != i
            printname = "$name@_$i"
            idx > 0 && (printnames[idx] = "$name@_$idx")
            names[name] = 0
        else
            printname = name
        end
        printnames[i] = printname
    end
    return printnames
end

function show(io::IO, l::Core.MethodInstance)
    def = l.def
    if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            print(io, "MethodInstance generator for ")
            show(io, def)
        else
            print(io, "MethodInstance for ")
            show_tuple_as_call(io, def.name, l.specTypes)
        end
    else
        print(io, "Toplevel MethodInstance thunk")
    end
end

function show_delim_array(io::IO, itr::Union{AbstractArray,SimpleVector}, op, delim, cl,
                          delim_one, i1=first(LinearIndices(itr)), l=last(LinearIndices(itr)))
    print(io, op)
    if !show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        if !haskey(io, :compact)
            recur_io = IOContext(recur_io, :compact => true)
        end
        first = true
        i = i1
        if l >= i1
            while true
                if !isassigned(itr, i)
                    print(io, undef_ref_str)
                else
                    x = itr[i]
                    show(recur_io, x)
                end
                i += 1
                if i > l
                    delim_one && first && print(io, delim)
                    break
                end
                first = false
                print(io, delim)
                print(io, ' ')
            end
        end
    end
    print(io, cl)
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one, i1=1, n=typemax(Int))
    print(io, op)
    if !show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        y = iterate(itr)
        first = true
        i0 = i1-1
        while i1 > 2 && y !== nothing
            y = iterate(itr, y[2])
            i1 -= 1
        end
        if y !== nothing
            typeinfo = get(io, :typeinfo, Any)
            while true
                x = y[1]
                y = iterate(itr, y[2])
                show(IOContext(recur_io, :typeinfo => itr isa typeinfo <: Tuple ?
                                             fieldtype(typeinfo, i1+i0) :
                                             typeinfo),
                     x)
                i1 += 1
                if y === nothing || i1 > n
                    delim_one && first && print(io, delim)
                    break
                end
                first = false
                print(io, delim)
                print(io, ' ')
            end
        end
    end
    print(io, cl)
end

show_comma_array(io::IO, itr, o, c) = show_delim_array(io, itr, o, ',', c, false)
show(io::IO, t::Tuple) = show_delim_array(io, t, '(', ',', ')', true)
show(io::IO, v::SimpleVector) = show_delim_array(io, v, "svec(", ',', ')', false)

show(io::IO, s::Symbol) = show_unquoted_quote_expr(io, s, 0, 0)

## Abstract Syntax Tree (AST) printing ##

# Summary:
#   print(io, ex) defers to show_unquoted(io, ex)
#   show(io, ex) defers to show_unquoted(io, QuoteNode(ex))
#   show_unquoted(io, ex) does the heavy lifting
#
# AST printing should follow two rules:
#   1. Meta.parse(string(ex)) == ex
#   2. eval(Meta.parse(repr(ex))) == ex
#
# Rule 1 means that printing an expression should generate Julia code which
# could be reparsed to obtain the original expression. This code should be
# unambiguous and as readable as possible.
#
# Rule 2 means that showing an expression should generate a quoted version of
# print’s output. Parsing and then evaling this output should return the
# original expression.
#
# This is consistent with many other show methods, i.e.:
#   show(Set([1,2,3]))                     # ==> "Set{Int64}([2,3,1])"
#   eval(Meta.parse("Set{Int64}([2,3,1])”) # ==> An actual set
# While this isn’t true of ALL show methods, it is of all ASTs.

const ExprNode = Union{Expr, QuoteNode, Slot, LineNumberNode, SSAValue,
                       GotoNode, GlobalRef, PhiNode, PhiCNode, UpsilonNode,
                       Core.Compiler.GotoIfNot, Core.Compiler.ReturnNode}
# Operators have precedence levels from 1-N, and show_unquoted defaults to a
# precedence level of 0 (the fourth argument). The top-level print and show
# methods use a precedence of -1 to specially allow space-separated macro syntax
print(        io::IO, ex::ExprNode)    = (show_unquoted(io, ex, 0, -1); nothing)
show(         io::IO, ex::ExprNode)    = show_unquoted_quote_expr(io, ex, 0, -1)
show_unquoted(io::IO, ex)              = show_unquoted(io, ex, 0, 0)
show_unquoted(io::IO, ex, indent::Int) = show_unquoted(io, ex, indent, 0)
show_unquoted(io::IO, ex, ::Int,::Int) = show(io, ex)

## AST printing constants ##

const indent_width = 4
const quoted_syms = Set{Symbol}([:(:),:(::),:(:=),:(=),:(==),:(===),:(=>)])
const uni_syms = Set{Symbol}([:(::), :(<:), :(>:)])
const uni_ops = Set{Symbol}([:(+), :(-), :(!), :(¬), :(~), :(<:), :(>:), :(√), :(∛), :(∜)])
const expr_infix_wide = Set{Symbol}([
    :(=), :(+=), :(-=), :(*=), :(/=), :(\=), :(^=), :(&=), :(|=), :(÷=), :(%=), :(>>>=), :(>>=), :(<<=),
    :(.=), :(.+=), :(.-=), :(.*=), :(./=), :(.\=), :(.^=), :(.&=), :(.|=), :(.÷=), :(.%=), :(.>>>=), :(.>>=), :(.<<=),
    :(&&), :(||), :(<:), :($=), :(⊻=)])
const expr_infix = Set{Symbol}([:(:), :(->), Symbol("::")])
const expr_infix_any = union(expr_infix, expr_infix_wide)
const expr_calls  = Dict(:call => ('(',')'), :calldecl => ('(',')'),
                         :ref => ('[',']'), :curly => ('{','}'), :(.) => ('(',')'))
const expr_parens = Dict(:tuple=>('(',')'), :vcat=>('[',']'),
                         :hcat =>('[',']'), :row =>('[',']'), :vect=>('[',']'),
                         :braces=>('{','}'), :bracescat=>('{','}'))

## AST decoding helpers ##

is_id_start_char(c::AbstractChar) = ccall(:jl_id_start_char, Cint, (UInt32,), c) != 0
is_id_char(c::AbstractChar) = ccall(:jl_id_char, Cint, (UInt32,), c) != 0
function isidentifier(s::AbstractString)
    isempty(s) && return false
    c, rest = Iterators.peel(s)
    is_id_start_char(c) || return false
    return all(is_id_char, rest)
end
isidentifier(s::Symbol) = isidentifier(string(s))

"""
    isoperator(s::Symbol)

Return `true` if the symbol can be used as an operator, `false` otherwise.

# Examples
```jldoctest
julia> Base.isoperator(:+), Base.isoperator(:f)
(true, false)
```
"""
isoperator(s::Symbol) = ccall(:jl_is_operator, Cint, (Cstring,), s) != 0

"""
    isunaryoperator(s::Symbol)

Return `true` if the symbol can be used as a unary (prefix) operator, `false` otherwise.

# Examples
```jldoctest
julia> Base.isunaryoperator(:-), Base.isunaryoperator(:√), Base.isunaryoperator(:f)
(true, true, false)
```
"""
isunaryoperator(s::Symbol) = ccall(:jl_is_unary_operator, Cint, (Cstring,), s) != 0
is_unary_and_binary_operator(s::Symbol) = ccall(:jl_is_unary_and_binary_operator, Cint, (Cstring,), s) != 0

"""
    isbinaryoperator(s::Symbol)

Return `true` if the symbol can be used as a binary (infix) operator, `false` otherwise.

# Examples
```jldoctest
julia> Base.isbinaryoperator(:-), Base.isbinaryoperator(:√), Base.isbinaryoperator(:f)
(true, false, false)
```
"""
isbinaryoperator(s::Symbol) = isoperator(s) && (!isunaryoperator(s) || is_unary_and_binary_operator(s))

"""
    operator_precedence(s::Symbol)

Return an integer representing the precedence of operator `s`, relative to
other operators. Higher-numbered operators take precedence over lower-numbered
operators. Return `0` if `s` is not a valid operator.

# Examples
```jldoctest
julia> Base.operator_precedence(:+), Base.operator_precedence(:*), Base.operator_precedence(:.)
(11, 13, 17)

julia> Base.operator_precedence(:sin), Base.operator_precedence(:+=), Base.operator_precedence(:(=))  # (Note the necessary parens on `:(=)`)
(0, 1, 1)
```
"""
operator_precedence(s::Symbol) = Int(ccall(:jl_operator_precedence, Cint, (Cstring,), s))
operator_precedence(x::Any) = 0 # fallback for generic expression nodes
const prec_assignment = operator_precedence(:(=))
const prec_pair = operator_precedence(:(=>))
const prec_control_flow = operator_precedence(:(&&))
const prec_arrow = operator_precedence(:(-->))
const prec_comparison = operator_precedence(:(>))
const prec_power = operator_precedence(:(^))
const prec_decl = operator_precedence(:(::))

"""
    operator_associativity(s::Symbol)

Return a symbol representing the associativity of operator `s`. Left- and right-associative
operators return `:left` and `:right`, respectively. Return `:none` if `s` is non-associative
or an invalid operator.

# Examples
```jldoctest
julia> Base.operator_associativity(:-), Base.operator_associativity(:+), Base.operator_associativity(:^)
(:left, :none, :right)

julia> Base.operator_associativity(:⊗), Base.operator_associativity(:sin), Base.operator_associativity(:→)
(:left, :none, :right)
```
"""
function operator_associativity(s::Symbol)
    if operator_precedence(s) in (prec_arrow, prec_assignment, prec_control_flow, prec_pair, prec_power) ||
        (isunaryoperator(s) && !is_unary_and_binary_operator(s)) || s === :<|
        return :right
    elseif operator_precedence(s) in (0, prec_comparison) || s in (:+, :++, :*)
        return :none
    end
    return :left
end

is_expr(ex, head::Symbol)         = (isa(ex, Expr) && (ex.head == head))
is_expr(ex, head::Symbol, n::Int) = is_expr(ex, head) && length(ex.args) == n

is_quoted(ex)            = false
is_quoted(ex::QuoteNode) = true
is_quoted(ex::Expr)      = is_expr(ex, :quote, 1) || is_expr(ex, :inert, 1)

unquoted(ex::QuoteNode)  = ex.value
unquoted(ex::Expr)       = ex.args[1]

## AST printing helpers ##

function printstyled end
function with_output_color end

const indent_width = 4

is_expected_union(u::Union) = u.a == Nothing || u.b == Nothing || u.a == Missing || u.b == Missing

emphasize(io, str::AbstractString, col = Base.error_color()) = get(io, :color, false) ?
    printstyled(io, str; color=col, bold=true) :
    print(io, uppercase(str))

show_linenumber(io::IO, line)       = print(io, "#= line ", line, " =#")
show_linenumber(io::IO, line, file) = print(io, "#= ", file, ":", line, " =#")
show_linenumber(io::IO, line, file::Nothing) = show_linenumber(io, line)

# show a block, e g if/for/etc
function show_block(io::IO, head, args::Vector, body, indent::Int)
    print(io, head)
    if !isempty(args)
        print(io, ' ')
        if head === :elseif
            show_list(io, args, " ", indent)
        else
            show_list(io, args, ", ", indent)
        end
    end

    ind = head === :module || head === :baremodule ? indent : indent + indent_width
    exs = is_expr(body, :block) ? body.args : Any[body]
    for ex in exs
        print(io, '\n', " "^ind)
        show_unquoted(io, ex, ind, -1)
    end
    print(io, '\n', " "^indent)
end
show_block(io::IO,head,    block,i::Int) = show_block(io,head, [], block,i)
function show_block(io::IO, head, arg, block, i::Int)
    if is_expr(arg, :block)
        show_block(io, head, arg.args, block, i)
    else
        show_block(io, head, Any[arg], block, i)
    end
end

# show an indented list
function show_list(io::IO, items, sep, indent::Int, prec::Int=0, enclose_operators::Bool=false)
    n = length(items)
    n == 0 && return
    indent += indent_width
    first = true
    for item in items
        !first && print(io, sep)
        parens = !is_quoted(item) &&
            (first && prec >= prec_power &&
             ((item isa Expr && item.head === :call && item.args[1] in uni_ops) ||
              (item isa Real && item < 0))) ||
              (enclose_operators && item isa Symbol && isoperator(item))
        parens && print(io, '(')
        show_unquoted(io, item, indent, parens ? 0 : prec)
        parens && print(io, ')')
        first = false
    end
end
# show an indented list inside the parens (op, cl)
function show_enclosed_list(io::IO, op, items, sep, cl, indent, prec=0, encl_ops=false)
    print(io, op)
    show_list(io, items, sep, indent, prec, encl_ops)
    print(io, cl)
end

# show a normal (non-operator) function call, e.g. f(x, y) or A[z]
function show_call(io::IO, head, func, func_args, indent)
    op, cl = expr_calls[head]
    if (isa(func, Symbol) && func !== :(:) && !(head === :. && isoperator(func))) ||
            (isa(func, Expr) && (func.head == :. || func.head == :curly))
        show_unquoted(io, func, indent)
    else
        print(io, '(')
        show_unquoted(io, func, indent)
        print(io, ')')
    end
    if head == :(.)
        print(io, '.')
    end
    if !isempty(func_args) && isa(func_args[1], Expr) && func_args[1].head === :parameters
        print(io, op)
        show_list(io, func_args[2:end], ", ", indent)
        print(io, "; ")
        show_list(io, func_args[1].args, ", ", indent)
        print(io, cl)
    else
        show_enclosed_list(io, op, func_args, ", ", cl, indent)
    end
end

## AST printing ##

show_unquoted(io::IO, val::SSAValue, ::Int, ::Int)      = print(io, "%", val.id)
show_unquoted(io::IO, sym::Symbol, ::Int, ::Int)        = print(io, sym)
show_unquoted(io::IO, ex::LineNumberNode, ::Int, ::Int) = show_linenumber(io, ex.line, ex.file)
show_unquoted(io::IO, ex::GotoNode, ::Int, ::Int)       = print(io, "goto %", ex.label)
function show_unquoted(io::IO, ex::GlobalRef, ::Int, ::Int)
    print(io, ex.mod)
    print(io, '.')
    quoted = !isidentifier(ex.name)
    parens = quoted && (!isoperator(ex.name) || (ex.name in quoted_syms))
    quoted && print(io, ':')
    parens && print(io, '(')
    print(io, ex.name)
    parens && print(io, ')')
    nothing
end

function show_unquoted(io::IO, ex::Slot, ::Int, ::Int)
    typ = isa(ex, TypedSlot) ? ex.typ : Any
    slotid = ex.id
    slotnames = get(io, :SOURCE_SLOTNAMES, false)
    if (isa(slotnames, Vector{String}) &&
        slotid <= length(slotnames::Vector{String}))
        print(io, (slotnames::Vector{String})[slotid])
    else
        print(io, "_", slotid)
    end
    if typ !== Any && isa(ex, TypedSlot)
        print(io, "::", typ)
    end
end

function show_unquoted(io::IO, ex::QuoteNode, indent::Int, prec::Int)
    if isa(ex.value, Symbol)
        show_unquoted_quote_expr(io, ex.value, indent, prec)
    else
        print(io, "\$(QuoteNode(")
        show(io, ex.value)
        print(io, "))")
    end
end

function show_unquoted_quote_expr(io::IO, @nospecialize(value), indent::Int, prec::Int)
    if isa(value, Symbol) && !(value in quoted_syms)
        s = string(value)
        if isidentifier(s) || isoperator(value)
            print(io, ":")
            print(io, value)
        else
            print(io, "Symbol(\"", escape_string(s), "\")")
        end
    else
        if isa(value,Expr) && value.head === :block
            show_block(io, "quote", value, indent)
            print(io, "end")
        else
            print(io, ":(")
            show_unquoted(io, value, indent+2, -1)  # +2 for `:(`
            print(io, ")")
        end
    end
end

function show_generator(io, ex, indent)
    if ex.head === :flatten
        fg = ex
        ranges = Any[]
        while isa(fg, Expr) && fg.head === :flatten
            push!(ranges, fg.args[1].args[2:end])
            fg = fg.args[1].args[1]
        end
        push!(ranges, fg.args[2:end])
        show_unquoted(io, fg.args[1], indent)
        for r in ranges
            print(io, " for ")
            show_list(io, r, ", ", indent)
        end
    else
        show_unquoted(io, ex.args[1], indent)
        print(io, " for ")
        show_list(io, ex.args[2:end], ", ", indent)
    end
end

function show_import_path(io::IO, ex)
    if !isa(ex, Expr)
        print(io, ex)
    elseif ex.head === :(:)
        show_import_path(io, ex.args[1])
        print(io, ": ")
        for i = 2:length(ex.args)
            if i > 2
                print(io, ", ")
            end
            show_import_path(io, ex.args[i])
        end
    elseif ex.head === :(.)
        print(io, ex.args[1])
        for i = 2:length(ex.args)
            if ex.args[i-1] != :(.)
                print(io, '.')
            end
            print(io, ex.args[i])
        end
    else
        show_unquoted(io, ex)
    end
end

# TODO: implement interpolated strings
function show_unquoted(io::IO, ex::Expr, indent::Int, prec::Int)
    head, args, nargs = ex.head, ex.args, length(ex.args)
    unhandled = false
    # dot (i.e. "x.y"), but not compact broadcast exps
    if head === :(.) && (length(args) != 2 || !is_expr(args[2], :tuple))
        if length(args) == 2 && is_quoted(args[2])
            item = args[1]
            # field
            field = unquoted(args[2])
            parens = !is_quoted(item) && !(item isa Symbol && isidentifier(item))
            parens && print(io, '(')
            show_unquoted(io, item, indent)
            parens && print(io, ')')
            # .
            print(io, '.')
            # item
            parens = !(field isa Symbol) || (field in quoted_syms)
            quoted = parens || isoperator(field)
            quoted && print(io, ':')
            parens && print(io, '(')
            show_unquoted(io, field, indent)
            parens && print(io, ')')
        else
            unhandled = true
        end

    # infix (i.e. "x <: y" or "x = y")
    elseif (head in expr_infix_any && nargs==2)
        func_prec = operator_precedence(head)
        head_ = head in expr_infix_wide ? " $head " : head
        if func_prec <= prec
            show_enclosed_list(io, '(', args, head_, ')', indent, func_prec, true)
        else
            show_list(io, args, head_, indent, func_prec, true)
        end

    # list (i.e. "(1, 2, 3)" or "[1, 2, 3]")
    elseif haskey(expr_parens, head)               # :tuple/:vcat
        op, cl = expr_parens[head]
        if head === :vcat || head === :bracescat
            sep = "; "
        elseif head === :hcat || head === :row
            sep = " "
        else
            sep = ", "
        end
        head !== :row && print(io, op)
        show_list(io, args, sep, indent)
        if nargs == 1
            if head === :tuple
                print(io, ',')
            elseif head === :vcat
                print(io, ';')
            end
        end
        head !== :row && print(io, cl)

    # function call
    elseif head === :call && nargs >= 1
        func = args[1]
        fname = isa(func, GlobalRef) ? func.name : func
        func_prec = operator_precedence(fname)
        if func_prec > 0 || fname in uni_ops
            func = fname
        end
        func_args = args[2:end]

        # scalar multiplication (i.e. "100x")
        if (func === :* &&
            length(func_args)==2 && isa(func_args[1], Real) && isa(func_args[2], Symbol))
            if func_prec <= prec
                show_enclosed_list(io, '(', func_args, "", ')', indent, func_prec)
            else
                show_list(io, func_args, "", indent, func_prec)
            end

        # unary operator (i.e. "!z")
        elseif isa(func,Symbol) && func in uni_ops && length(func_args) == 1
            show_unquoted(io, func, indent)
            arg1 = func_args[1]
            if isa(arg1, Expr) || (isa(arg1, Symbol) && isoperator(arg1))
                show_enclosed_list(io, '(', func_args, ", ", ')', indent, func_prec)
            else
                show_unquoted(io, arg1, indent, func_prec)
            end

        # binary operator (i.e. "x + y")
        elseif func_prec > 0 # is a binary operator
            na = length(func_args)
            if (na == 2 || (na > 2 && func in (:+, :++, :*)) || (na == 3 && func === :(:))) &&
                    all(!isa(a, Expr) || a.head !== :... for a in func_args)
                sep = func === :(:) ? "$func" : " $func "

                if func_prec <= prec
                    show_enclosed_list(io, '(', func_args, sep, ')', indent, func_prec, true)
                else
                    show_list(io, func_args, sep, indent, func_prec, true)
                end
            elseif na == 1
                # 1-argument call to normally-binary operator
                op, cl = expr_calls[head]
                print(io, "(")
                show_unquoted(io, func, indent)
                print(io, ")")
                show_enclosed_list(io, op, func_args, ", ", cl, indent)
            else
                show_call(io, head, func, func_args, indent)
            end

        # normal function (i.e. "f(x,y)")
        else
            show_call(io, head, func, func_args, indent)
        end

    # new expr
    elseif head === :new
        show_enclosed_list(io, "%new(", args, ", ", ")", indent)

    # other call-like expressions ("A[1,2]", "T{X,Y}", "f.(X,Y)")
    elseif haskey(expr_calls, head) && nargs >= 1  # :ref/:curly/:calldecl/:(.)
        funcargslike = head == :(.) ? args[2].args : args[2:end]
        show_call(io, head, args[1], funcargslike, indent)

    # comprehensions
    elseif head === :typed_comprehension && length(args) == 2
        show_unquoted(io, args[1], indent)
        print(io, '[')
        show_generator(io, args[2], indent)
        print(io, ']')

    elseif head === :comprehension && length(args) == 1
        print(io, '[')
        show_generator(io, args[1], indent)
        print(io, ']')

    elseif (head === :generator && length(args) >= 2) || (head === :flatten && length(args) == 1)
        print(io, '(')
        show_generator(io, ex, indent)
        print(io, ')')

    elseif head === :filter && length(args) == 2
        show_unquoted(io, args[2], indent)
        print(io, " if ")
        show_unquoted(io, args[1], indent)

    # comparison (i.e. "x < y < z")
    elseif head === :comparison && nargs >= 3 && (nargs&1==1)
        comp_prec = minimum(operator_precedence, args[2:2:end])
        if comp_prec <= prec
            show_enclosed_list(io, '(', args, " ", ')', indent, comp_prec)
        else
            show_list(io, args, " ", indent, comp_prec)
        end

    # function calls need to transform the function from :call to :calldecl
    # so that operators are printed correctly
    elseif head === :function && nargs==2 && is_expr(args[1], :call)
        show_block(io, head, Expr(:calldecl, args[1].args...), args[2], indent)
        print(io, "end")

    elseif head === :function && nargs == 1
        print(io, "function ", args[1], " end")

    elseif head === :do && nargs == 2
        show_unquoted(io, args[1], indent, -1)
        print(io, " do ")
        show_list(io, args[2].args[1].args, ", ", 0)
        for stmt in args[2].args[2].args
            print(io, '\n', " "^(indent + indent_width))
            show_unquoted(io, stmt, indent + indent_width, -1)
        end
        print(io, '\n', " "^indent)
        print(io, "end")

    # block with argument
    elseif head in (:for,:while,:function,:if,:elseif,:let) && nargs==2
        show_block(io, head, args[1], args[2], indent)
        print(io, "end")

    elseif (head === :if || head === :elseif) && nargs == 3
        show_block(io, head, args[1], args[2], indent)
        if isa(args[3],Expr) && args[3].head == :elseif
            show_unquoted(io, args[3], indent, prec)
        else
            show_block(io, "else", args[3], indent)
            print(io, "end")
        end

    elseif head === :module && nargs==3 && isa(args[1],Bool)
        show_block(io, args[1] ? :module : :baremodule, args[2], args[3], indent)
        print(io, "end")

    # type declaration
    elseif head === :struct && nargs==3
        show_block(io, args[1] ? Symbol("mutable struct") : Symbol("struct"), args[2], args[3], indent)
        print(io, "end")

    elseif head === :primitive && nargs == 2
        print(io, "primitive type ")
        show_list(io, args, ' ', indent)
        print(io, " end")

    elseif head === :abstract && nargs == 1
        print(io, "abstract type ")
        show_list(io, args, ' ', indent)
        print(io, " end")

    # empty return (i.e. "function f() return end")
    elseif head === :return && nargs == 1 && args[1] === nothing
        print(io, head)

    # type annotation (i.e. "::Int")
    elseif head in uni_syms && nargs == 1
        print(io, head)
        show_unquoted(io, args[1], indent)

    # var-arg declaration or expansion
    # (i.e. "function f(L...) end" or "f(B...)")
    elseif head === :(...) && nargs == 1
        show_unquoted(io, args[1], indent)
        print(io, "...")

    elseif (nargs == 0 && head in (:break, :continue))
        print(io, head)

    elseif (nargs == 1 && head in (:return, :const)) ||
                          head in (:local,  :global, :export)
        print(io, head, ' ')
        show_list(io, args, ", ", indent)

    elseif head === :macrocall && nargs >= 2
        # first show the line number argument as a comment
        if isa(args[2], LineNumberNode) || is_expr(args[2], :line)
            print(io, args[2], ' ')
        end
        # Use the functional syntax unless specifically designated with prec=-1
        # and hide the line number argument from the argument list
        if prec >= 0
            show_call(io, :call, args[1], args[3:end], indent)
        else
            show_args = Vector{Any}(undef, length(args) - 1)
            show_args[1] = args[1]
            show_args[2:end] = args[3:end]
            show_list(io, show_args, ' ', indent)
        end

    elseif head === :line && 1 <= nargs <= 2
        show_linenumber(io, args...)

    elseif head === :try && 3 <= nargs <= 4
        show_block(io, "try", args[1], indent)
        if is_expr(args[3], :block)
            show_block(io, "catch", args[2] === false ? Any[] : args[2], args[3], indent)
        end
        if nargs >= 4 && is_expr(args[4], :block)
            show_block(io, "finally", Any[], args[4], indent)
        end
        print(io, "end")

    elseif head === :block
        show_block(io, "begin", ex, indent)
        print(io, "end")

    elseif head === :quote && nargs == 1 && isa(args[1], Symbol)
        show_unquoted_quote_expr(io, args[1]::Symbol, indent, 0)

    elseif head === :gotoifnot && nargs == 2 && isa(args[2], Int)
        print(io, "unless ")
        show_unquoted(io, args[1], indent, 0)
        print(io, " goto %")
        print(io, args[2]::Int)

    elseif head === :string && nargs == 1 && isa(args[1], AbstractString)
        show(io, args[1])

    elseif head === :null
        print(io, "nothing")

    elseif head === :kw && nargs == 2
        show_unquoted(io, args[1], indent+indent_width)
        print(io, '=')
        show_unquoted(io, args[2], indent+indent_width)

    elseif head === :string
        print(io, '"')
        for x in args
            if !isa(x,AbstractString)
                print(io, "\$(")
                if isa(x,Symbol) && !(x in quoted_syms)
                    print(io, x)
                else
                    show_unquoted(io, x)
                end
                print(io, ")")
            else
                escape_string(io, x, "\"\$")
            end
        end
        print(io, '"')

    elseif (head === :&#= || head === :$=#) && length(args) == 1
        print(io, head)
        a1 = args[1]
        parens = (isa(a1,Expr) && a1.head !== :tuple) || (isa(a1,Symbol) && isoperator(a1))
        parens && print(io, "(")
        show_unquoted(io, a1)
        parens && print(io, ")")

    # transpose
    elseif head === Symbol('\'') && length(args) == 1
        if isa(args[1], Symbol)
            show_unquoted(io, args[1])
        else
            print(io, "(")
            show_unquoted(io, args[1])
            print(io, ")")
        end
        print(io, head)

    # `where` syntax
    elseif head === :where && length(args) > 1
        parens = 1 <= prec
        parens && print(io, "(")
        show_unquoted(io, args[1], indent, operator_precedence(:(::)))
        print(io, " where ")
        if nargs == 2
            show_unquoted(io, args[2], indent, 1)
        else
            print(io, "{")
            show_list(io, args[2:end], ", ", indent)
            print(io, "}")
        end
        parens && print(io, ")")

    elseif head === :import || head === :using
        print(io, head)
        print(io, ' ')
        first = true
        for a in args
            if !first
                print(io, ", ")
            end
            first = false
            show_import_path(io, a)
        end
    elseif head === :meta && length(args) >= 2 && args[1] === :push_loc
        print(io, "# meta: location ", join(args[2:end], " "))
    elseif head === :meta && length(args) == 1 && args[1] === :pop_loc
        print(io, "# meta: pop location")
    elseif head === :meta && length(args) == 2 && args[1] === :pop_loc
        print(io, "# meta: pop locations ($(args[2]))")
    # print anything else as "Expr(head, args...)"
    else
        unhandled = true
    end
    if unhandled
        print(io, "\$(Expr(")
        show(io, ex.head)
        for arg in args
            print(io, ", ")
            show(io, arg)
        end
        print(io, "))")
    end
    nothing
end

function show_tuple_as_call(io::IO, name::Symbol, sig::Type)
    # print a method signature tuple for a lambda definition
    color = get(io, :color, false) && get(io, :backtrace, false) ? stackframe_function_color() : :nothing
    if sig === Tuple
        printstyled(io, name, "(...)", color=color)
        return
    end
    sig = unwrap_unionall(sig).parameters
    with_output_color(color, io) do io
        ft = sig[1]
        uw = unwrap_unionall(ft)
        if ft <: Function && isa(uw,DataType) && isempty(uw.parameters) &&
                isdefined(uw.name.module, uw.name.mt.name) &&
                ft == typeof(getfield(uw.name.module, uw.name.mt.name))
            print(io, uw.name.mt.name)
        elseif isa(ft, DataType) && ft.name === Type.body.name && !Core.Compiler.has_free_typevars(ft)
            f = ft.parameters[1]
            print(io, f)
        else
            print(io, "(::", ft, ")")
        end
    end
    first = true
    print_style = get(io, :color, false) && get(io, :backtrace, false) ? :bold : :nothing
    printstyled(io, "(", color=print_style)
    for i = 2:length(sig)  # fixme (iter): `eachindex` with offset?
        first || print(io, ", ")
        first = false
        print(io, "::", sig[i])
    end
    printstyled(io, ")", color=print_style)
    nothing
end

resolvebinding(@nospecialize(ex)) = ex
resolvebinding(ex::QuoteNode) = ex.value
resolvebinding(ex::Symbol) = resolvebinding(GlobalRef(Main, ex))
function resolvebinding(ex::Expr)
    if ex.head == :. && isa(ex.args[2], Symbol)
        parent = resolvebinding(ex.args[1])
        if isa(parent, Module)
            return resolvebinding(GlobalRef(parent, ex.args[2]))
        end
    end
    return nothing
end
function resolvebinding(ex::GlobalRef)
    isdefined(ex.mod, ex.name) || return nothing
    isconst(ex.mod, ex.name) || return nothing
    m = getfield(ex.mod, ex.name)
    isa(m, Module) || return nothing
    return m
end

function ismodulecall(ex::Expr)
    return ex.head == :call && (ex.args[1] === GlobalRef(Base,:getfield) ||
                                ex.args[1] === GlobalRef(Core,:getfield)) &&
           isa(resolvebinding(ex.args[2]), Module)
end

function show(io::IO, tv::TypeVar)
    # If we are in the `unionall_env`, the type-variable is bound
    # and the type constraints are already printed.
    # We don't need to print it again.
    # Otherwise, the lower bound should be printed if it is not `Bottom`
    # and the upper bound should be printed if it is not `Any`.
    in_env = (:unionall_env => tv) in io
    function show_bound(io::IO, @nospecialize(b))
        parens = isa(b,UnionAll) && !print_without_params(b)
        parens && print(io, "(")
        show(io, b)
        parens && print(io, ")")
    end
    lb, ub = tv.lb, tv.ub
    if !in_env && lb !== Bottom
        if ub === Any
            write(io, tv.name)
            print(io, ">:")
            show_bound(io, lb)
        else
            show_bound(io, lb)
            print(io, "<:")
            write(io, tv.name)
        end
    else
        write(io, tv.name)
    end
    if !in_env && ub !== Any
        print(io, "<:")
        show_bound(io, ub)
    end
    nothing
end

module IRShow
    const Compiler = Core.Compiler
    using Core.IR
    import ..Base
    import .Compiler: IRCode, ReturnNode, GotoIfNot, CFG, scan_ssa_use!, Argument, isexpr, compute_basic_blocks, block_for_inst
    Base.size(r::Compiler.StmtRange) = Compiler.size(r)
    Base.show(io::IO, r::Compiler.StmtRange) = print(io, Compiler.first(r):Compiler.last(r))
    include("compiler/ssair/show.jl")
end

function show(io::IO, src::CodeInfo)
    # Fix slot names and types in function body
    print(io, "CodeInfo(")
    lambda_io::IOContext = io
    if src.slotnames !== nothing
        lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => sourceinfo_slotnames(src))
    end
    @assert src.codelocs !== nothing
    if isempty(src.linetable) || src.linetable[1] isa LineInfoNode
        println(io)
        # TODO: static parameter values?
        IRShow.show_ir(lambda_io, src)
    else
        # this is a CodeInfo that has not been used as a method yet, so its locations are still LineNumberNodes
        body = Expr(:block)
        body.args = src.code
        show(lambda_io, body)
    end
    print(io, ")")
end


function dump(io::IOContext, x::SimpleVector, n::Int, indent)
    if isempty(x)
        print(io, "empty SimpleVector")
        return
    end
    print(io, "SimpleVector")
    if n > 0
        for i = 1:length(x)
            println(io)
            print(io, indent, "  ", i, ": ")
            if isassigned(x,i)
                dump(io, x[i], n - 1, string(indent, "  "))
            else
                print(io, undef_ref_str)
            end
        end
    end
    nothing
end

function dump(io::IOContext, @nospecialize(x), n::Int, indent)
    T = typeof(x)
    if isa(x, Function)
        print(io, x, " (function of type ", T, ")")
    else
        print(io, T)
    end
    nf = nfields(x)
    if nf > 0
        if n > 0 && !show_circular(io, x)
            recur_io = IOContext(io, Pair{Symbol,Any}(:SHOWN_SET, x))
            for field in 1:nf
                println(io)
                fname = string(fieldname(T, field))
                print(io, indent, "  ", fname, ": ")
                if isdefined(x,field)
                    dump(recur_io, getfield(x, field), n - 1, string(indent, "  "))
                else
                    print(io, undef_ref_str)
                end
            end
        end
    elseif !isa(x, Function)
        print(io, " ")
        show(io, x)
    end
    nothing
end

dump(io::IOContext, x::Module, n::Int, indent) = print(io, "Module ", x)
dump(io::IOContext, x::String, n::Int, indent) = (print(io, "String "); show(io, x))
dump(io::IOContext, x::Symbol, n::Int, indent) = print(io, typeof(x), " ", x)
dump(io::IOContext, x::Union,  n::Int, indent) = print(io, x)
dump(io::IOContext, x::Ptr,    n::Int, indent) = print(io, x)

function dump_elts(io::IOContext, x::Array, n::Int, indent, i0, i1)
    for i in i0:i1
        print(io, indent, "  ", i, ": ")
        if !isassigned(x,i)
            print(io, undef_ref_str)
        else
            dump(io, x[i], n - 1, string(indent, "  "))
        end
        i < i1 && println(io)
    end
end

function dump(io::IOContext, x::Array, n::Int, indent)
    print(io, "Array{$(eltype(x))}($(size(x)))")
    if eltype(x) <: Number
        print(io, " ")
        show(io, x)
    else
        if n > 0 && !isempty(x) && !show_circular(io, x)
            println(io)
            recur_io = IOContext(io, :SHOWN_SET => x)
            lx = length(x)
            if get(io, :limit, false)
                dump_elts(recur_io, x, n, indent, 1, (lx <= 10 ? lx : 5))
                if lx > 10
                    println(io)
                    println(io, indent, "  ...")
                    dump_elts(recur_io, x, n, indent, lx - 4, lx)
                end
            else
                dump_elts(recur_io, x, n, indent, 1, lx)
            end
        end
    end
    nothing
end

# Types
function dump(io::IOContext, x::DataType, n::Int, indent)
    print(io, x)
    if x !== Any
        print(io, " <: ", supertype(x))
    end
    if n > 0 && !(x <: Tuple) && !x.abstract
        tvar_io::IOContext = io
        for tparam in x.parameters
            # approximately recapture the list of tvar parameterization
            # that may be used by the internal fields
            if isa(tparam, TypeVar)
                tvar_io = IOContext(tvar_io, :unionall_env => tparam)
            end
        end
        fields = fieldnames(x)
        fieldtypes = x.types
        for idx in 1:length(fields)
            println(io)
            print(io, indent, "  ", fields[idx], "::")
            print(tvar_io, fieldtypes[idx])
        end
    end
    nothing
end

const DUMP_DEFAULT_MAXDEPTH = 8

function dump(io::IO, @nospecialize(x); maxdepth=DUMP_DEFAULT_MAXDEPTH)
    dump(IOContext(io), x, maxdepth, "")
    println(io)
end

"""
    dump(x; maxdepth=$DUMP_DEFAULT_MAXDEPTH)

Show every part of the representation of a value.
The depth of the output is truncated at `maxdepth`.

# Examples
```jldoctest
julia> struct MyStruct
           x
           y
       end

julia> x = MyStruct(1, (2,3));

julia> dump(x)
MyStruct
  x: Int64 1
  y: Tuple{Int64,Int64}
    1: Int64 2
    2: Int64 3

julia> dump(x; maxdepth = 1)
MyStruct
  x: Int64 1
  y: Tuple{Int64,Int64}
```
"""
dump(arg; maxdepth=DUMP_DEFAULT_MAXDEPTH) = dump(IOContext(stdout::IO, :limit => true), arg; maxdepth=maxdepth)


"""
`alignment(X)` returns a tuple (left,right) showing how many characters are
needed on either side of an alignment feature such as a decimal point.
"""
alignment(io::IO, x::Any) = (0, length(sprint(show, x, context=io, sizehint=0)))
alignment(io::IO, x::Number) = (length(sprint(show, x, context=io, sizehint=0)), 0)
"`alignment(42)` yields (2,0)"
alignment(io::IO, x::Integer) = (length(sprint(show, x, context=io, sizehint=0)), 0)
"`alignment(4.23)` yields (1,3) for `4` and `.23`"
function alignment(io::IO, x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(show, x, context=io, sizehint=0))
    m === nothing ? (length(sprint(show, x, context=io, sizehint=0)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
"`alignment(1 + 10im)` yields (3,5) for `1 +` and `_10im` (plus sign on left, space on right)"
function alignment(io::IO, x::Complex)
    m = match(r"^(.*[^e][\+\-])(.*)$", sprint(show, x, context=io, sizehint=0))
    m === nothing ? (length(sprint(show, x, context=io, sizehint=0)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(io::IO, x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(show, x, context=io, sizehint=0))
    m === nothing ? (length(sprint(show, x, context=io, sizehint=0)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end

function alignment(io::IO, x::Pair)
    s = sprint(show, x, context=io, sizehint=0)
    if !isdelimited(io, x) # i.e. use "=>" for display
        iocompact = IOContext(io, :compact => get(io, :compact, true),
                                  :typeinfo => gettypeinfos(io, x)[1])
        left = length(sprint(show, x.first, context=iocompact, sizehint=0))
        left += 2 * !isdelimited(iocompact, x.first) # for parens around p.first
        left += !get(io, :compact, false) # spaces are added around "=>"
        (left+1, length(s)-left-1) # +1 for the "=" part of "=>"
    else
        (0, length(s)) # as for x::Any
    end
end

const undef_ref_str = "#undef"


"""
    summary(io::IO, x)
    str = summary(x)

Print to a stream `io`, or return a string `str`, giving a brief description of
a value. By default returns `string(typeof(x))`, e.g. [`Int64`](@ref).

For arrays, returns a string of size and type info,
e.g. `10-element Array{Int64,1}`.

# Examples
```jldoctest
julia> summary(1)
"Int64"

julia> summary(zeros(2))
"2-element Array{Float64,1}"
```
"""
summary(io::IO, x) = print(io, typeof(x))
function summary(x)
    io = IOBuffer()
    summary(io, x)
    String(take!(io))
end

## `summary` for AbstractArrays
# sizes such as 0-dimensional, 4-dimensional, 2x3
dims2string(d) = isempty(d) ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), '×')

inds2string(inds) = join(map(_indsstring,inds), '×')
_indsstring(i) = string(i)
_indsstring(i::Slice) = string(i.indices)

# anything array-like gets summarized e.g. 10-element Array{Int64,1}
summary(io::IO, a::AbstractArray) = summary(io, a, axes(a))
function summary(io::IO, a, inds::Tuple{Vararg{OneTo}})
    print(io, dims2string(length.(inds)), " ")
    showarg(io, a, true)
end
function summary(io::IO, a, inds)
    showarg(io, a, true)
    print(io, " with indices ", inds2string(inds))
end

"""
    showarg(io::IO, x, toplevel)

Show `x` as if it were an argument to a function. This function is
used by [`summary`](@ref) to display type information in terms of sequences of
function calls on objects. `toplevel` is `true` if this is
the direct call from `summary` and `false` for nested (recursive) calls.

The fallback definition is to print `x` as "::\\\$(typeof(x))",
representing argument `x` in terms of its type. (The double-colon is
omitted if `toplevel=true`.) However, you can
specialize this function for specific types to customize printing.

# Example

A SubArray created as `view(a, :, 3, 2:5)`, where `a` is a
3-dimensional Float64 array, has type

    SubArray{Float64,2,Array{Float64,3},Tuple{Colon,Int64,UnitRange{Int64}},false}

The default `show` printing would display this full type.
However, the summary for SubArrays actually prints as

    2×4 view(::Array{Float64,3}, :, 3, 2:5) with eltype Float64

because of a definition similar to

    function Base.showarg(io::IO, v::SubArray, toplevel)
        print(io, "view(")
        showarg(io, parent(v), false)
        print(io, ", ", join(v.indices, ", "))
        print(io, ')')
        toplevel && print(io, " with eltype ", eltype(v))
    end

Note that we're calling `showarg` recursively for the parent array
type, indicating that any recursed calls are not at the top level.
Printing the parent as `::Array{Float64,3}` is the fallback (non-toplevel)
behavior, because no specialized method for `Array` has been defined.
"""
function showarg(io::IO, ::Type{T}, toplevel) where {T}
    toplevel || print(io, "::")
    print(io, "Type{", T, "}")
end
function showarg(io::IO, x, toplevel)
    toplevel || print(io, "::")
    print(io, typeof(x))
end
# This method resolves an ambiguity for packages that specialize on eltype
function showarg(io::IO, a::Array{Union{}}, toplevel)
    toplevel || print(io, "::")
    print(io, typeof(a))
end

# Container specializations
function showarg(io::IO, v::SubArray, toplevel)
    print(io, "view(")
    showarg(io, parent(v), false)
    showindices(io, v.indices...)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(v))
end
showindices(io, ::Slice, inds...) =
    (print(io, ", :"); showindices(io, inds...))
showindices(io, ind1, inds...) =
    (print(io, ", ", ind1); showindices(io, inds...))
showindices(io) = nothing

function showarg(io::IO, r::ReshapedArray, toplevel)
    print(io, "reshape(")
    showarg(io, parent(r), false)
    print(io, ", ", join(r.dims, ", "))
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(r))
end

function showarg(io::IO, r::ReinterpretArray{T}, toplevel) where {T}
    print(io, "reinterpret($T, ")
    showarg(io, parent(r), false)
    print(io, ')')
end

# pretty printing for Iterators.Pairs
function Base.showarg(io::IO, r::Iterators.Pairs{<:Integer, <:Any, <:Any, T}, toplevel) where T<:AbstractArray
    print(io, "pairs(IndexLinear(), ::$T)")
end

function Base.showarg(io::IO, r::Iterators.Pairs{Symbol, <:Any, <:Any, T}, toplevel) where {T <: NamedTuple}
    print(io, "pairs(::NamedTuple)")
end

function Base.showarg(io::IO, r::Iterators.Pairs{<:Any, <:Any, I, D}, toplevel) where {D, I}
    print(io, "Iterators.Pairs(::$D, ::$I)")
end

# printing BitArrays

# (following functions not exported - mainly intended for debug)

function print_bit_chunk(io::IO, c::UInt64, l::Integer = 64)
    for s = 0:l-1
        d = (c >>> s) & 1
        print(io, "01"[d + 1])
        if (s + 1) & 7 == 0
            print(io, " ")
        end
    end
end

print_bit_chunk(c::UInt64, l::Integer) = print_bit_chunk(stdout, c, l)
print_bit_chunk(c::UInt64) = print_bit_chunk(stdout, c)

function bitshow(io::IO, B::BitArray)
    isempty(B) && return
    Bc = B.chunks
    for i = 1:length(Bc)-1
        print_bit_chunk(io, Bc[i])
        print(io, ": ")
    end
    l = _mod64(length(B)-1) + 1
    print_bit_chunk(io, Bc[end], l)
end
bitshow(B::BitArray) = bitshow(stdout, B)

bitstring(B::BitArray) = sprint(bitshow, B)
