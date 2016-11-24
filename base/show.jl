# This file is a part of Julia. License is MIT: http://julialang.org/license

print(io::IO, s::Symbol) = (write(io,s); nothing)

"""
    IOContext

`IOContext` provides a mechanism for passing output configuration settings among [`show`](@ref) methods.

In short, it is an immutable dictionary that is a subclass of `IO`. It supports standard
dictionary operations such as [`getindex`](@ref), and can also be used as an I/O stream.
"""
immutable IOContext{IO_t <: IO} <: AbstractPipe
    io::IO_t
    dict::ImmutableDict{Symbol, Any}
    function IOContext(io::IO_t, dict::ImmutableDict{Symbol, Any})
        assert(!(IO_t <: IOContext))
        return new(io, dict)
    end
end

"""
    IOContext(io::IO; properties...)

The same as `IOContext(io::IO, KV::Pair)`, but accepting properties as keyword arguments.
"""
IOContext(io::IO; kws...) = IOContext(IOContext(io, ImmutableDict{Symbol,Any}()); kws...)
function IOContext(io::IOContext; kws...)
    for (k, v) in kws
        io = IOContext(io, k, v)
    end
    io
end

IOContext(io::IOContext, dict::ImmutableDict) = typeof(io)(io.io, dict)
IOContext(io::IO, dict::ImmutableDict) = IOContext{typeof(io)}(io, dict)

IOContext(io::IO, key, value) = IOContext(io, ImmutableDict{Symbol, Any}(key, value))
IOContext(io::IOContext, key, value) = IOContext(io, ImmutableDict{Symbol, Any}(io.dict, key, value))

IOContext(io::IO, context::IO) = IOContext(io)

"""
    IOContext(io::IO, context::IOContext)

Create an `IOContext` that wraps an alternate `IO` but inherits the properties of `context`.
"""
IOContext(io::IO, context::IOContext) = IOContext(io, context.dict)

"""
    IOContext(io::IO, KV::Pair)

Create an `IOContext` that wraps a given stream, adding the specified `key=>value` pair to
the properties of that stream (note that `io` can itself be an `IOContext`).

 - use `(key => value) in dict` to see if this particular combination is in the properties set
 - use `get(dict, key, default)` to retrieve the most recent value for a particular key

The following properties are in common use:

 - `:compact`: Boolean specifying that small values should be printed more compactly, e.g.
   that numbers should be printed with fewer digits. This is set when printing array
   elements.
 - `:limit`: Boolean specifying that containers should be truncated, e.g. showing `…` in
   place of most elements.
 - `:displaysize`: A `Tuple{Int,Int}` giving the size in rows and columns to use for text
   output. This can be used to override the display size for called functions, but to
   get the size of the screen use the `displaysize` function.
"""
IOContext(io::IO, KV::Pair) = IOContext(io, KV[1], KV[2])

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
_limit_output = nothing # TODO: delete with with_output_limit deprecation
function get(io::IOContext, key, default)
    if key === :limit && _limit_output !== nothing
        default = _limit_output::Bool
    end
    get(io.dict, key, default)
end
function get(io::IO, key, default)
    if key === :limit && _limit_output !== nothing
        return _limit_output::Bool
    end
    default
end

displaysize(io::IOContext) = haskey(io, :displaysize) ? io[:displaysize] : displaysize(io.io)

show_circular(io::IO, x::ANY) = false
function show_circular(io::IOContext, x::ANY)
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

show(io::IO, x::ANY) = show_default(io, x)
function show_default(io::IO, x::ANY)
    t = typeof(x)::DataType
    show(io, t)
    print(io, '(')
    nf = nfields(t)
    nb = sizeof(x)
    if nf != 0 || nb==0
        if !show_circular(io, x)
            recur_io = IOContext(io, :SHOWN_SET => x)
            for i=1:nf
                f = fieldname(t, i)
                if !isdefined(x, f)
                    print(io, undef_ref_str)
                else
                    show(recur_io, getfield(x, f))
                end
                if i < nf
                    print(io, ',')
                end
            end
        end
    else
        print(io, "0x")
        p = data_pointer_from_objref(x)
        for i=nb-1:-1:0
            print(io, hex(unsafe_load(convert(Ptr{UInt8}, p+i)), 2))
        end
    end
    print(io,')')
end

# Check if a particular symbol is exported from a standard library module
function is_exported_from_stdlib(name::Symbol, mod::Module)
    !isdefined(mod, name) && return false
    orig = getfield(mod, name)
    while !(mod === Base || mod === Core)
        parent = module_parent(mod)
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
    if !isdefined(mt, :module) || is_exported_from_stdlib(mt.name, mt.module) || mt.module === Main
        print(io, mt.name)
    else
        print(io, mt.module, ".", mt.name)
    end
end

function show(io::IO, x::Core.IntrinsicFunction)
    name = ccall(:jl_intrinsic_name, Cstring, (Core.IntrinsicFunction,), x)
    print(io, unsafe_string(name))
end

show(io::IO, ::Core.BottomType) = print(io, "Union{}")

function show(io::IO, x::Union)
    print(io, "Union")
    sorted_types = sort!(uniontypes(x); by=string)
    show_comma_array(io, sorted_types, '{', '}')
end

function print_without_params(x::ANY)
    if isa(x,UnionAll)
        b = unwrap_unionall(x)
        return isa(b,DataType) && b.name.wrapper === x
    end
    return false
end

function show(io::IO, x::UnionAll)
    if print_without_params(x)
        return show(io, unwrap_unionall(x).name)
    end
    tvar_env = get(io, :tvar_env, false)
    if tvar_env !== false && isa(tvar_env, AbstractVector)
        tvar_env = Any[tvar_env..., x.var]
    else
        tvar_env = Any[x.var]
    end
    show(IOContext(io, tvar_env = tvar_env), x.body)
    print(io, " where ")
    show(io, x.var)
end

function show_type_parameter(io::IO, p::ANY, has_tvar_env::Bool)
    if has_tvar_env
        show(io, p)
    else
        show(IOContext(io, :tvar_env, true), p)
    end
end

show(io::IO, x::DataType) = show_datatype(io, x)

function show_datatype(io::IO, x::DataType)
    # tvar_env is a `::Vector{Any}` when we are printing a method signature
    # and `true` if we are printing type parameters outside a method signature.
    has_tvar_env = get(io, :tvar_env, false) !== false

    if (!isempty(x.parameters) || x.name === Tuple.name) && x !== Tuple
        n = length(x.parameters)

        # Print homogeneous tuples with more than 3 elements compactly as NTuple{N, T}
        if n > 3 && all(i -> (x.parameters[1] === i), x.parameters)
            print(io, "NTuple{", n, ',', x.parameters[1], "}")
        else
            show(io, x.name)
            # Do not print the type parameters for the primary type if we are
            # printing a method signature or type parameter.
            # Always print the type parameter if we are printing the type directly
            # since this information is still useful.
            print(io, '{')
            for (i, p) in enumerate(x.parameters)
                show_type_parameter(io, p, has_tvar_env)
                i < n && print(io, ',')
            end
            print(io, '}')
        end
    else
        show(io, x.name)
    end
end

macro show(exs...)
    blk = Expr(:block)
    for ex in exs
        push!(blk.args, :(println($(sprint(show_unquoted,ex)*" = "),
                                  repr(begin value=$(esc(ex)) end))))
    end
    if !isempty(exs); push!(blk.args, :value); end
    return blk
end

function show(io::IO, tn::TypeName)
    if is_exported_from_stdlib(tn.name, tn.module) || tn.module === Main
        print(io, tn.name)
    else
        print(io, tn.module, '.', tn.name)
    end
end

show(io::IO, ::Void) = print(io, "nothing")
show(io::IO, b::Bool) = print(io, b ? "true" : "false")
show(io::IO, n::Signed) = (write(io, dec(n)); nothing)
show(io::IO, n::Unsigned) = print(io, "0x", hex(n,sizeof(n)<<1))
print(io::IO, n::Unsigned) = print(io, dec(n))

show{T}(io::IO, p::Ptr{T}) = print(io, typeof(p), " @0x$(hex(UInt(p), Sys.WORD_SIZE>>2))")

function show(io::IO, p::Pair)
    if typeof(p.first) != typeof(p).parameters[1] ||
       typeof(p.second) != typeof(p).parameters[2]
        return show_default(io, p)
    end

    isa(p.first,Pair) && print(io, "(")
    show(io, p.first)
    isa(p.first,Pair) && print(io, ")")
    print(io, "=>")
    isa(p.second,Pair) && print(io, "(")
    show(io, p.second)
    isa(p.second,Pair) && print(io, ")")
    nothing
end

function show(io::IO, m::Module)
    if m === Main
        print(io, "Main")
    else
        print(io, join(fullname(m),"."))
    end
end

function sourceinfo_slotnames(src::CodeInfo)
    slotnames = src.slotnames
    isa(slotnames, Array) || return String[]
    names = Dict{String,Int}()
    printnames = Vector{String}(length(slotnames))
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
    if isdefined(l, :def)
        if l.def.isstaged && l === l.def.generator
            print(io, "MethodInstance generator for ")
            show(io, l.def)
        else
            print(io, "MethodInstance for ")
            show_lambda_types(io, l)
        end
    else
        print(io, "Toplevel MethodInstance thunk")
    end
end

function show(io::IO, src::CodeInfo)
    # Fix slot names and types in function body
    print(io, "CodeInfo(")
    if isa(src.code, Array{Any,1})
        lambda_io = IOContext(io, :SOURCEINFO => src)
        if src.slotnames !== nothing
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => sourceinfo_slotnames(src))
        end
        body = Expr(:body)
        body.args = src.code
        show(lambda_io, body)
    else
        print(io, "<compressed>")
    end
    print(io, ")")
end

function show_delim_array(io::IO, itr::Union{AbstractArray,SimpleVector}, op, delim, cl,
                          delim_one, i1=first(linearindices(itr)), l=last(linearindices(itr)))
    print(io, op)
    if !show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        if !haskey(io, :compact)
            recur_io = IOContext(recur_io, :compact => true)
        end
        newline = true
        first = true
        i = i1
        if l >= i1
            while true
                if !isassigned(itr, i)
                    print(io, undef_ref_str)
                    multiline = false
                else
                    x = itr[i]
                    multiline = isa(x,AbstractArray) && ndims(x)>1 && !isempty(x)
                    newline && multiline && println(io)
                    show(recur_io, x)
                end
                i += 1
                if i > l
                    delim_one && first && print(io, delim)
                    break
                end
                first = false
                print(io, delim)
                if multiline
                    println(io); println(io)
                    newline = false
                else
                    newline = true
                end
            end
        end
    end
    print(io, cl)
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one, i1=1, n=typemax(Int))
    print(io, op)
    if !show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        state = start(itr)
        newline = true
        first = true
        while i1 > 1 && !done(itr,state)
            _, state = next(itr, state)
            i1 -= 1
        end
        if !done(itr,state)
            while true
                x, state = next(itr,state)
                multiline = isa(x,AbstractArray) && ndims(x)>1 && !isempty(x)
                newline && multiline && println(io)
                show(recur_io, x)
                i1 += 1
                if done(itr,state) || i1 > n
                    delim_one && first && print(io, delim)
                    break
                end
                first = false
                print(io, delim)
                if multiline
                    println(io); println(io)
                    newline = false
                else
                    newline = true
                end
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
#   1. parse(string(ex)) == ex
#   2. eval(parse(repr(ex))) == ex
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
#   show(Set([1,2,3]))                # ==> "Set{Int64}([2,3,1])"
#   eval(parse("Set{Int64}([2,3,1])”) # ==> An actual set
# While this isn’t true of ALL show methods, it is of all ASTs.

typealias ExprNode Union{Expr, QuoteNode, Slot, LineNumberNode,
                         LabelNode, GotoNode, GlobalRef}
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
const quoted_syms = Set{Symbol}([:(:),:(::),:(:=),:(=),:(==),:(!=),:(===),:(!==),:(=>),:(>=),:(<=)])
const uni_ops = Set{Symbol}([:(+), :(-), :(!), :(¬), :(~), :(<:), :(>:), :(√), :(∛), :(∜)])
const expr_infix_wide = Set{Symbol}([
    :(=), :(+=), :(-=), :(*=), :(/=), :(\=), :(^=), :(&=), :(|=), :(÷=), :(%=), :(>>>=), :(>>=), :(<<=),
    :(.=), :(.+=), :(.-=), :(.*=), :(./=), :(.\=), :(.^=), :(.&=), :(.|=), :(.÷=), :(.%=), :(.>>>=), :(.>>=), :(.<<=),
    :(&&), :(||), :(<:), :(=>), :($=), :(⊻=)]) # `$=` should be removed after deprecation is removed, issue #18977
const expr_infix = Set{Symbol}([:(:), :(->), Symbol("::")])
const expr_infix_any = union(expr_infix, expr_infix_wide)
const all_ops = union(quoted_syms, uni_ops, expr_infix_any)
const expr_calls  = Dict(:call => ('(',')'), :calldecl => ('(',')'),
                         :ref => ('[',']'), :curly => ('{','}'), :(.) => ('(',')'))
const expr_parens = Dict(:tuple=>('(',')'), :vcat=>('[',']'),
                         :hcat =>('[',']'), :row =>('[',']'), :vect=>('[',']'))

## AST decoding helpers ##

is_id_start_char(c::Char) = ccall(:jl_id_start_char, Cint, (UInt32,), c) != 0
is_id_char(c::Char) = ccall(:jl_id_char, Cint, (UInt32,), c) != 0
function isidentifier(s::AbstractString)
    i = start(s)
    done(s, i) && return false
    (c, i) = next(s, i)
    is_id_start_char(c) || return false
    while !done(s, i)
        (c, i) = next(s, i)
        is_id_char(c) || return false
    end
    return true
end
isidentifier(s::Symbol) = isidentifier(string(s))

isoperator(s::Symbol) = ccall(:jl_is_operator, Cint, (Cstring,), s) != 0
operator_precedence(s::Symbol) = Int(ccall(:jl_operator_precedence, Cint, (Cstring,), s))
operator_precedence(x::Any) = 0 # fallback for generic expression nodes
const prec_power = operator_precedence(:(^))
const prec_decl = operator_precedence(:(::))

is_expr(ex, head::Symbol)         = (isa(ex, Expr) && (ex.head == head))
is_expr(ex, head::Symbol, n::Int) = is_expr(ex, head) && length(ex.args) == n

is_linenumber(ex::LineNumberNode) = true
is_linenumber(ex::Expr)           = (ex.head == :line)
is_linenumber(ex)                 = false

is_quoted(ex)            = false
is_quoted(ex::QuoteNode) = true
is_quoted(ex::Expr)      = is_expr(ex, :quote, 1) || is_expr(ex, :inert, 1)

unquoted(ex::QuoteNode)  = ex.value
unquoted(ex::Expr)       = ex.args[1]

## AST printing helpers ##

typeemphasize(io::IO) = get(io, :TYPEEMPHASIZE, false) === true

const indent_width = 4

function show_expr_type(io::IO, ty, emph)
    if ty === Function
        print(io, "::F")
    elseif ty === Core.IntrinsicFunction
        print(io, "::I")
    else
        if emph && (!isleaftype(ty) || ty == Core.Box)
            emphasize(io, "::$ty")
        else
            print(io, "::$ty")
        end
    end
end

emphasize(io, str::AbstractString) = have_color ? print_with_color(Base.error_color(), io, str; bold = true) : print(io, uppercase(str))

show_linenumber(io::IO, line)       = print(io," # line ",line,':')
show_linenumber(io::IO, line, file) = print(io," # ", file,", line ",line,':')

# show a block, e g if/for/etc
function show_block(io::IO, head, args::Vector, body, indent::Int)
    print(io, head, ' ')
    show_list(io, args, ", ", indent)

    ind = head === :module || head === :baremodule ? indent : indent + indent_width
    exs = (is_expr(body, :block) || is_expr(body, :body)) ? body.args : Any[body]
    for ex in exs
        if !is_linenumber(ex); print(io, '\n', " "^ind); end
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
    if n == 0; return end
    indent += indent_width
    first = true
    for item in items
        !first && print(io, sep)
        parens = enclose_operators && isa(item,Symbol) && isoperator(item)
        parens && print(io, '(')
        show_unquoted(io, item, indent, prec)
        parens && print(io, ')')
        first = false
    end
end
# show an indented list inside the parens (op, cl)
function show_enclosed_list(io::IO, op, items, sep, cl, indent, prec=0, encl_ops=false)
    print(io, op); show_list(io, items, sep, indent, prec, encl_ops); print(io, cl)
end

# show a normal (non-operator) function call, e.g. f(x,y) or A[z]
function show_call(io::IO, head, func, func_args, indent)
    op, cl = expr_calls[head]
    if isa(func, Symbol) || (isa(func, Expr) &&
            (func.head == :. || func.head == :curly))
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
        show_list(io, func_args[2:end], ',', indent)
        print(io, "; ")
        show_list(io, func_args[1].args, ',', indent)
        print(io, cl)
    else
        show_enclosed_list(io, op, func_args, ",", cl, indent)
    end
end

## AST printing ##

show_unquoted(io::IO, sym::Symbol, ::Int, ::Int)        = print(io, sym)
show_unquoted(io::IO, ex::LineNumberNode, ::Int, ::Int) = show_linenumber(io, ex.line)
show_unquoted(io::IO, ex::LabelNode, ::Int, ::Int)      = print(io, ex.label, ": ")
show_unquoted(io::IO, ex::GotoNode, ::Int, ::Int)       = print(io, "goto ", ex.label)
show_unquoted(io::IO, ex::GlobalRef, ::Int, ::Int)      = print(io, ex.mod, '.', ex.name)

function show_unquoted(io::IO, ex::Slot, ::Int, ::Int)
    typ = isa(ex,TypedSlot) ? ex.typ : Any
    slotid = ex.id
    src = get(io, :SOURCEINFO, false)
    if isa(src, CodeInfo)
        slottypes = (src::CodeInfo).slottypes
        if isa(slottypes, Array) && slotid <= length(slottypes::Array)
            slottype = slottypes[slotid]
            # The Slot in assignment can somehow have an Any type
            if slottype <: typ
                typ = slottype
            end
        end
    end
    slotnames = get(io, :SOURCE_SLOTNAMES, false)
    if (isa(slotnames, Vector{String}) &&
        slotid <= length(slotnames::Vector{String}))
        print(io, (slotnames::Vector{String})[slotid])
    else
        print(io, "_", slotid)
    end
    emphstate = typeemphasize(io)
    if emphstate || (typ !== Any && isa(ex,TypedSlot))
        show_expr_type(io, typ, emphstate)
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

function show_unquoted_quote_expr(io::IO, value, indent::Int, prec::Int)
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
            show_unquoted(io, value, indent+indent_width, -1)
            print(io, ")")
        end
    end
end

function show_generator(io, ex, indent)
    if ex.head === :flatten
        fg = ex
        ranges = Any[]
        while isa(fg, Expr) && fg.head === :flatten
            push!(ranges, fg.args[1].args[2])
            fg = fg.args[1].args[1]
        end
        push!(ranges, fg.args[2])
        show_unquoted(io, fg.args[1], indent)
        for r in ranges
            print(io, " for ")
            show_unquoted(io, r, indent)
        end
    else
        show_unquoted(io, ex.args[1], indent)
        print(io, " for ")
        show_unquoted(io, ex.args[2], indent)
        for i = 3:length(ex.args)
            print(io, ", ")
            show_unquoted(io, ex.args[i], indent)
        end
    end
end

# TODO: implement interpolated strings
function show_unquoted(io::IO, ex::Expr, indent::Int, prec::Int)
    head, args, nargs = ex.head, ex.args, length(ex.args)
    emphstate = typeemphasize(io)
    show_type = true
    if (ex.head == :(=) || ex.head == :line ||
        ex.head == :boundscheck ||
        ex.head == :gotoifnot ||
        ex.head == :return)
        show_type = false
    end
    if !emphstate && ex.typ === Any
        show_type = false
    end
    # dot (i.e. "x.y"), but not compact broadcast exps
    if head === :(.) && !is_expr(args[2], :tuple)
        show_unquoted(io, args[1], indent + indent_width)
        print(io, '.')
        if is_quoted(args[2])
            show_unquoted(io, unquoted(args[2]), indent + indent_width)
        else
            print(io, '(')
            show_unquoted(io, args[2], indent + indent_width)
            print(io, ')')
        end

    # infix (i.e. "x<:y" or "x = y")
    elseif (head in expr_infix_any && nargs==2) || (head === :(:) && nargs==3)
        func_prec = operator_precedence(head)
        head_ = head in expr_infix_wide ? " $head " : head
        if func_prec <= prec
            show_enclosed_list(io, '(', args, head_, ')', indent, func_prec, true)
        else
            show_list(io, args, head_, indent, func_prec, true)
        end

    # list (i.e. "(1,2,3)" or "[1,2,3]")
    elseif haskey(expr_parens, head)               # :tuple/:vcat
        op, cl = expr_parens[head]
        if head === :vcat
            sep = ";"
        elseif head === :hcat || head === :row
            sep = " "
        else
            sep = ","
        end
        head !== :row && print(io, op)
        show_list(io, args, sep, indent)
        if (head === :tuple || head === :vcat) && nargs == 1
            print(io, sep)
        end
        head !== :row && print(io, cl)

    # function call
    elseif head === :call && nargs >= 1
        func = args[1]
        fname = isa(func,GlobalRef) ? func.name : func
        func_prec = operator_precedence(fname)
        if func_prec > 0 || fname in uni_ops
            func = fname
        end
        func_args = args[2:end]

        if (in(ex.args[1], (GlobalRef(Base, :box), :throw)) ||
            ismodulecall(ex))
            show_type = false
        end
        if show_type
            prec = prec_decl
        end

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
            if isa(func_args[1], Expr) || func_args[1] in all_ops
                show_enclosed_list(io, '(', func_args, ",", ')', indent, func_prec)
            else
                show_unquoted(io, func_args[1])
            end

        # binary operator (i.e. "x + y")
        elseif func_prec > 0 # is a binary operator
            na = length(func_args)
            if (na == 2 || (na > 2 && func in (:+, :++, :*))) &&
                    all(!isa(a, Expr) || a.head !== :... for a in func_args)
                sep = " $func "
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
                show_enclosed_list(io, op, func_args, ",", cl, indent)
            else
                show_call(io, head, func, func_args, indent)
            end

        # normal function (i.e. "f(x,y)")
        else
            show_call(io, head, func, func_args, indent)
        end

    # other call-like expressions ("A[1,2]", "T{X,Y}", "f.(X,Y)")
    elseif haskey(expr_calls, head) && nargs >= 1  # :ref/:curly/:calldecl/:(.)
        funcargslike = head == :(.) ? ex.args[2].args : ex.args[2:end]
        show_call(io, head, ex.args[1], funcargslike, indent)

    # comprehensions
    elseif (head === :typed_comprehension || head === :typed_dict_comprehension) && length(args) == 2
        isdict = (head === :typed_dict_comprehension)
        isdict && print(io, '(')
        show_unquoted(io, args[1], indent)
        isdict && print(io, ')')
        print(io, '[')
        show_generator(io, args[2], indent)
        print(io, ']')

    elseif (head === :comprehension || head === :dict_comprehension) && length(args) == 1
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

    # block with argument
    elseif head in (:for,:while,:function,:if) && nargs==2
        show_block(io, head, args[1], args[2], indent)
        print(io, "end")

    elseif head === :module && nargs==3 && isa(args[1],Bool)
        show_block(io, args[1] ? :module : :baremodule, args[2], args[3], indent)
        print(io, "end")

    # type declaration
    elseif head === :type && nargs==3
        show_block(io, args[1] ? :type : :immutable, args[2], args[3], indent)
        print(io, "end")

    elseif head === :bitstype && nargs == 2
        print(io, "bitstype ")
        show_list(io, args, ' ', indent)

    # empty return (i.e. "function f() return end")
    elseif head === :return && nargs == 1 && args[1] === nothing
        print(io, head)

    # type annotation (i.e. "::Int")
    elseif head === Symbol("::") && nargs == 1
        print(io, "::")
        show_unquoted(io, args[1], indent)

    # var-arg declaration or expansion
    # (i.e. "function f(L...) end" or "f(B...)")
    elseif head === :(...) && nargs == 1
        show_unquoted(io, args[1], indent)
        print(io, "...")

    elseif (nargs == 0 && head in (:break, :continue))
        print(io, head)

    elseif (nargs == 1 && head in (:return, :abstract, :const)) ||
                          head in (:local,  :global, :export)
        print(io, head, ' ')
        show_list(io, args, ", ", indent)

    elseif head === :macrocall && nargs >= 1
        # Use the functional syntax unless specifically designated with prec=-1
        if prec >= 0
            show_call(io, :call, ex.args[1], ex.args[2:end], indent)
        else
            show_list(io, args, ' ', indent)
        end

    elseif head === :typealias && nargs == 2
        print(io, "typealias ")
        show_list(io, args, ' ', indent)

    elseif head === :line && 1 <= nargs <= 2
        show_linenumber(io, args...)

    elseif head === :if && nargs == 3     # if/else
        show_block(io, "if",   args[1], args[2], indent)
        show_block(io, "else", args[3], indent)
        print(io, "end")

    elseif head === :try && 3 <= nargs <= 4
        show_block(io, "try", args[1], indent)
        if is_expr(args[3], :block)
            show_block(io, "catch", args[2] === false ? Any[] : args[2], args[3], indent)
        end
        if nargs >= 4 && is_expr(args[4], :block)
            show_block(io, "finally", Any[], args[4], indent)
        end
        print(io, "end")

    elseif head === :let && nargs >= 1
        show_block(io, "let", args[2:end], args[1], indent); print(io, "end")

    elseif head === :block || head === :body
        show_block(io, "begin", ex, indent); print(io, "end")

    elseif head === :quote && nargs == 1 && isa(args[1],Symbol)
        show_unquoted_quote_expr(io, args[1], indent, 0)

    elseif head === :gotoifnot && nargs == 2
        print(io, "unless ")
        show_list(io, args, " goto ", indent)

    elseif head === :string && nargs == 1 && isa(args[1], AbstractString)
        show(io, args[1])

    elseif head === :null
        print(io, "nothing")

    elseif head === :kw && length(args)==2
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
    elseif (head === Symbol('\'') || head === Symbol(".'")) && length(args) == 1
        if isa(args[1], Symbol)
            show_unquoted(io, args[1])
        else
            print(io, "(")
            show_unquoted(io, args[1])
            print(io, ")")
        end
        print(io, head)

    # `where` syntax
    elseif head === :where && length(args) == 2
        parens = 1 <= prec
        parens && print(io, "(")
        show_unquoted(io, args[1], indent, operator_precedence(:(::)))
        print(io, " where ")
        show_unquoted(io, args[2], indent, 1)
        parens && print(io, ")")

    elseif head === :import || head === :importall || head === :using
        print(io, head)
        first = true
        for a = args
            if first
                print(io, ' ')
                first = false
            else
                print(io, '.')
            end
            if a !== :.
                print(io, a)
            end
        end
    elseif head === :meta && length(args) >= 2 && args[1] === :push_loc
        print(io, "# meta: location ", join(args[2:end], " "))
        show_type = false
    elseif head === :meta && length(args) == 1 && args[1] === :pop_loc
        print(io, "# meta: pop location")
        show_type = false
    # print anything else as "Expr(head, args...)"
    else
        show_type = false
        if emphstate && ex.head !== :lambda && ex.head !== :method
            io = IOContext(io, :TYPEEMPHASIZE => false)
            emphstate = false
        end
        print(io, "\$(Expr(")
        show(io, ex.head)
        for arg in args
            print(io, ", ")
            show(io, arg)
        end
        print(io, "))")
    end
    show_type && show_expr_type(io, ex.typ, emphstate)
    nothing
end

function show_lambda_types(io::IO, li::Core.MethodInstance)
    # print a method signature tuple for a lambda definition
    local sig
    returned_from_do = false
    Base.with_output_color(have_color ? stackframe_function_color() : :nothing, io) do io
        if li.specTypes === Tuple
            print(io, li.def.name, "(...)")
            returned_from_do = true
            return
        end
        sig = li.specTypes.parameters
        ft = sig[1]
        if ft <: Function && isempty(ft.parameters) &&
                isdefined(ft.name.module, ft.name.mt.name) &&
                ft == typeof(getfield(ft.name.module, ft.name.mt.name))
            print(io, ft.name.mt.name)
        elseif isa(ft, DataType) && ft.name === Type.body.name && isleaftype(ft)
            f = ft.parameters[1]
            print(io, f)
        else
            print(io, "(::", ft, ")")
        end
    end
    returned_from_do && return
    first = true
    print_style = have_color ? :bold : :nothing
    print_with_color(print_style, io, "(")
    for i = 2:length(sig)  # fixme (iter): `eachindex` with offset?
        first || print(io, ", ")
        first = false
        print(io, "::", sig[i])
    end
    print_with_color(print_style, io, ")")
    nothing
end

function ismodulecall(ex::Expr)
    return ex.head == :call && (ex.args[1] === GlobalRef(Base,:getfield) ||
                                ex.args[1] === GlobalRef(Core,:getfield)) &&
        isa(ex.args[2], Symbol) &&
        isdefined(current_module(), ex.args[2]) &&
        isa(getfield(current_module(), ex.args[2]), Module)
end

function show(io::IO, tv::TypeVar)
    # If `tvar_env` exist and we are in it, the type constraint are
    # already printed and we don't need to print it again.
    # Otherwise, the lower bound should be printed if it is not `Bottom`
    # and the upper bound should be printed if it is not `Any`.
    tvar_env = isa(io, IOContext) && get(io, :tvar_env, false)
    if isa(tvar_env, Vector{Any})
        in_env = (tv in tvar_env::Vector{Any})
    else
        in_env = false
    end
    function show_bound(io::IO, b::ANY)
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

function dump(io::IO, x::SimpleVector, n::Int, indent)
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

function dump(io::IO, x::ANY, n::Int, indent)
    T = typeof(x)
    if isa(x, Function)
        print(io, x, " (function of type ", T, ")")
    else
        print(io, T)
    end
    if nfields(T) > 0
        if n > 0
            for field in (isa(x,Tuple) ? (1:length(x)) : fieldnames(T))
                println(io)
                print(io, indent, "  ", field, ": ")
                if isdefined(x,field)
                    dump(io, getfield(x, field), n - 1, string(indent, "  "))
                else
                    print(io, undef_ref_str)
                end
            end
        end
    else
        !isa(x,Function) && print(io, " ", x)
    end
    nothing
end

dump(io::IO, x::Module, n::Int, indent) = print(io, "Module ", x)
dump(io::IO, x::String, n::Int, indent) = (print(io, "String "); show(io, x))
dump(io::IO, x::Symbol, n::Int, indent) = print(io, typeof(x), " ", x)
dump(io::IO, x::Union,  n::Int, indent) = print(io, x)

function dump_elts(io::IO, x::Array, n::Int, indent, i0, i1)
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

function dump(io::IO, x::Array, n::Int, indent)
    print(io, "Array{$(eltype(x))}($(size(x)))")
    if eltype(x) <: Number
        print(io, " ")
        show(io, x)
    else
        if n > 0 && !isempty(x)
            println(io)
            if get(io, :limit, false)
                dump_elts(io, x, n, indent, 1, (length(x) <= 10 ? length(x) : 5))
                if length(x) > 10
                    println(io)
                    println(io, indent, "  ...")
                    dump_elts(io, x, n, indent, length(x)-4, length(x))
                end
            else
                dump_elts(io, x, n, indent, 1, length(x))
            end
        end
    end
    nothing
end

# Types
function dump(io::IO, x::DataType, n::Int, indent)
    print(io, x)
    if x !== Any
        print(io, " <: ", supertype(x))
    end
    if !(x <: Tuple)
        tvar_io = IOContext(io, :tvar_env => Any[x.parameters...])
        fields = fieldnames(x)
        if n > 0
            for idx in 1:length(fields)
                println(io)
                print(io, indent, "  ", fields[idx], "::")
                print(tvar_io, fieldtype(x, idx))
            end
        end
    end
    nothing
end

# dumptype is for displaying abstract type hierarchies,
# based on Jameson Nash's examples/typetree.jl
function dumptype(io::IO, x::ANY, n::Int, indent)
    print(io, x)
    n == 0 && return  # too deeply nested
    isa(x, DataType) && x.abstract && dumpsubtypes(io, x, Main, n, indent)
    nothing
end

directsubtype(a::DataType, b::DataType) = supertype(a).name === b.name
directsubtype(a::UnionAll, b::DataType) = directsubtype(a.body, b)
directsubtype(a::Union, b::DataType) = directsubtype(a.a, b) || directsubtype(a.b, b)
# Fallback to handle TypeVar's
directsubtype(a, b::DataType) = false
function dumpsubtypes(io::IO, x::DataType, m::Module, n::Int, indent)
    for s in names(m, true)
        if isdefined(m, s) && !isdeprecated(m, s)
            t = getfield(m, s)
            if t === x || t === m
                continue
            elseif isa(t, Module) && module_name(t) === s && module_parent(t) === m
                # recurse into primary module bindings
                dumpsubtypes(io, x, t, n, indent)
            elseif isa(t, UnionAll) && directsubtype(t::UnionAll, x)
                println(io)
                print(io, indent, "  ", m, ".", s)
                print(io, " = ", t)
            elseif isa(t, Union) && directsubtype(t::Union, x)
                println(io)
                print(io, indent, "  ", m, ".", s, " = ", t)
            elseif isa(t, DataType) && directsubtype(t::DataType, x)
                println(io)
                if t.name.module !== m || t.name.name != s
                    # aliases to types
                    print(io, indent, "  ", m, ".", s, " = ", t)
                else
                    # primary type binding
                    print(io, indent, "  ")
                    dumptype(io, t, n - 1, string(indent, "  "))
                end
            end
        end
    end
    nothing
end


# For abstract types, use _dumptype only if it's a form that will be called
# interactively.
dump(io::IO, x::DataType; maxdepth=8) = ((x.abstract ? dumptype : dump)(io, x, maxdepth, ""); println(io))

dump(io::IO, arg; maxdepth=8) = (dump(io, arg, maxdepth, ""); println(io))
dump(arg; maxdepth=8) = dump(IOContext(STDOUT::IO, :limit => true), arg; maxdepth=maxdepth)


"""
`alignment(X)` returns a tuple (left,right) showing how many characters are
needed on either side of an alignment feature such as a decimal point.
"""
alignment(io::IO, x::Any) = (0, length(sprint(0, show, x, env=io)))
alignment(io::IO, x::Number) = (length(sprint(0, show, x, env=io)), 0)
"`alignment(42)` yields (2,0)"
alignment(io::IO, x::Integer) = (length(sprint(0, show, x, env=io)), 0)
"`alignment(4.23)` yields (1,3) for `4` and `.23`"
function alignment(io::IO, x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(0, show, x, env=io))
    m === nothing ? (length(sprint(0, show, x, env=io)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
"`alignment(1 + 10im)` yields (3,5) for `1 +` and `_10im` (plus sign on left, space on right)"
function alignment(io::IO, x::Complex)
    m = match(r"^(.*[\+\-])(.*)$", sprint(0, show, x, env=io))
    m === nothing ? (length(sprint(0, show, x, env=io)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(io::IO, x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(0, show, x, env=io))
    m === nothing ? (length(sprint(0, show, x, env=io)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end

const undef_ref_str = "#undef"
const undef_ref_alignment = (3,3)

"""
`alignment(X, rows, cols, cols_if_complete, cols_otherwise, sep)` returns the
alignment for specified parts of array `X`, returning the (left,right) info.
It will look in X's `rows`, `cols` (both lists of indices)
and figure out what's needed to be fully aligned, for example looking all
the way down a column and finding out the maximum size of each element.
Parameter `sep::Integer` is number of spaces to put between elements.
`cols_if_complete` and `cols_otherwise` indicate screen width to use.
Alignment is reported as a vector of (left,right) tuples, one for each
column going across the screen.
"""
function alignment(io::IO, X::AbstractVecOrMat,
        rows::AbstractVector, cols::AbstractVector,
        cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer)
    a = Tuple{Int, Int}[]
    for j in cols # need to go down each column one at a time
        l = r = 0
        for i in rows # plumb down and see what largest element sizes are
            if isassigned(X,i,j)
                aij = alignment(io, X[i,j])
            else
                aij = undef_ref_alignment
            end
            l = max(l, aij[1]) # left characters
            r = max(r, aij[2]) # right characters
        end
        push!(a, (l, r)) # one tuple per column of X, pruned to screen width
        if length(a) > 1 && sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop!(a) # remove this latest tuple if we're already beyond screen width
            break
        end
    end
    if 1 < length(a) < length(indices(X,2))
        while sum(map(sum,a)) + sep*length(a) >= cols_otherwise
            pop!(a)
        end
    end
    return a
end

"""
Unexported convenience function used in body of `replace_in_print_matrix`
methods. By default returns a string of the same width as original with a
centered cdot, used in printing of structural zeros of structured matrices.
Accept keyword args `c` for alternate single character marker.
"""
function replace_with_centered_mark(s::AbstractString;c::Char = '⋅')
    N = length(s)
    return join(setindex!([" " for i=1:N],string(c),ceil(Int,N/2)))
end

"""
`print_matrix_row(io, X, A, i, cols, sep)` produces the aligned output for
a single matrix row X[i, cols] where the desired list of columns is given.
The corresponding alignment A is used, and the separation between elements
is specified as string sep.
`print_matrix_row` will also respect compact output for elements.
"""
function print_matrix_row(io::IO,
        X::AbstractVecOrMat, A::Vector,
        i::Integer, cols::AbstractVector, sep::AbstractString)
    isempty(A) || first(indices(cols,1)) == 1 || throw(DimensionMismatch("indices of cols ($(indices(cols,1))) must start at 1"))
    for k = 1:length(A)
        j = cols[k]
        if isassigned(X,Int(i),Int(j)) # isassigned accepts only `Int` indices
            x = X[i,j]
            a = alignment(io, x)
            sx = sprint(0, show, x, env=io)
        else
            a = undef_ref_alignment
            sx = undef_ref_str
        end
        l = repeat(" ", A[k][1]-a[1]) # pad on left and right as needed
        r = repeat(" ", A[k][2]-a[2])
        prettysx = replace_in_print_matrix(X,i,j,sx)
        print(io, l, prettysx, r)
        if k < length(A); print(io, sep); end
    end
end

"""
`print_matrix_vdots` is used to show a series of vertical ellipsis instead
of a bunch of rows for long matrices. Not only is the string vdots shown
but it also repeated every M elements if desired.
"""
function print_matrix_vdots(io::IO, vdots::AbstractString,
        A::Vector, sep::AbstractString, M::Integer, m::Integer)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-length(vdots)))
            r = repeat(" ", max(0, w-length(vdots)-length(l)))
            print(io, l, vdots, r)
        else
            print(io, repeat(" ", w))
        end
        if k < length(A); print(io, sep); end
    end
end

"""
    print_matrix(io::IO, mat, pre, sep, post, hdots, vdots, ddots, hmod, vmod)

Prints a matrix with limited output size. If `io` sets `:limit` to true,
then only the corners of the matrix are printed, separated with vertical,
horizontal, and diagonal ellipses as appropriate.
Optional arguments are string pre (printed before the matrix, e.g. an opening bracket)
which will cause a corresponding same-size indent on following rows, and
string post (printed at the end of the last row of the matrix).
Also options to use different ellipsis characters hdots, vdots, ddots.
These are repeated every hmod or vmod elements.
"""
function print_matrix(io::IO, X::AbstractVecOrMat,
                      pre::AbstractString = " ",  # pre-matrix string
                      sep::AbstractString = "  ", # separator between elements
                      post::AbstractString = "",  # post-matrix string
                      hdots::AbstractString = "  \u2026  ",
                      vdots::AbstractString = "\u22ee",
                      ddots::AbstractString = "  \u22f1  ",
                      hmod::Integer = 5, vmod::Integer = 5)
    if !get(io, :limit, false)
        screenheight = screenwidth = typemax(Int)
    else
        sz = displaysize(io)
        screenheight, screenwidth = sz[1] - 4, sz[2]
    end
    screenwidth -= length(pre) + length(post)
    presp = repeat(" ", length(pre))  # indent each row to match pre string
    postsp = ""
    @assert strwidth(hdots) == strwidth(ddots)
    sepsize = length(sep)
    inds1, inds2 = indices(X,1), indices(X,2)
    m, n = length(inds1), length(inds2)
    rowsA, colsA = collect(inds1), collect(inds2)
    # To figure out alignments, only need to look at as many rows as could
    # fit down screen. If screen has at least as many rows as A, look at A.
    # If not, then we only need to look at the first and last chunks of A,
    # each half a screen height in size.
    halfheight = div(screenheight,2)
    if m > screenheight
        rowsA = [rowsA[1:halfheight]; rowsA[m-div(screenheight-1,2)+1:m]]
    end
    # Similarly for columns, only necessary to get alignments for as many
    # columns as could conceivably fit across the screen
    maxpossiblecols = div(screenwidth, 1+sepsize)
    if n > maxpossiblecols
        colsA = [colsA[1:maxpossiblecols]; colsA[(n-maxpossiblecols+1):n]]
    end
    A = alignment(io, X, rowsA, colsA, screenwidth, screenwidth, sepsize)
    # Nine-slicing is accomplished using print_matrix_row repeatedly
    if m <= screenheight # rows fit vertically on screen
        if n <= length(A) # rows and cols fit so just print whole matrix in one piece
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != last(rowsA); println(io, ); end
            end
        else # rows fit down screen but cols don't, so need horizontal ellipsis
            c = div(screenwidth-length(hdots)+1,2)+1  # what goes to right of ellipsis
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize)) # alignments for right
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize) # alignments for left of ellipsis
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,Lalign,i,colsA[1:length(Lalign)],sep)
                print(io, (i - first(rowsA)) % hmod == 0 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,Ralign,i,n-length(Ralign)+colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != last(rowsA); println(io, ); end
            end
        end
    else # rows don't fit so will need vertical ellipsis
        if n <= length(A) # rows don't fit, cols do, so only vertical ellipsis
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != rowsA[end]; println(io, ); end
                if i == rowsA[halfheight]
                    print(io, i == first(rowsA) ? pre : presp)
                    print_matrix_vdots(io, vdots,A,sep,vmod,1)
                    println(io, i == last(rowsA) ? post : postsp)
                end
            end
        else # neither rows nor cols fit, so use all 3 kinds of dots
            c = div(screenwidth-length(hdots)+1,2)+1
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize))
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize)
            r = mod((length(Ralign)-n+1),vmod) # where to put dots on right half
            for i in rowsA
                print(io, i == first(rowsA) ? pre : presp)
                print_matrix_row(io, X,Lalign,i,colsA[1:length(Lalign)],sep)
                print(io, (i - first(rowsA)) % hmod == 0 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,Ralign,i,n-length(Ralign)+colsA,sep)
                print(io, i == last(rowsA) ? post : postsp)
                if i != rowsA[end]; println(io, ); end
                if i == rowsA[halfheight]
                    print(io, i == first(rowsA) ? pre : presp)
                    print_matrix_vdots(io, vdots,Lalign,sep,vmod,1)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots,Ralign,sep,vmod,r)
                    println(io, i == last(rowsA) ? post : postsp)
                end
            end
        end
    end
end

"""
    summary(x)

Return a string giving a brief description of a value. By default returns
`string(typeof(x))`, e.g. `Int64`.

For arrays, returns a string of size and type info,
e.g. `10-element Array{Int64,1}`.

```jldoctest
julia> summary(1)
"Int64"

julia> summary(zeros(2))
"2-element Array{Float64,1}"
```
"""
summary(x) = string(typeof(x)) # e.g. Int64

# sizes such as 0-dimensional, 4-dimensional, 2x3
dims2string(d::Dims) = isempty(d) ? "0-dimensional" :
                       length(d) == 1 ? "$(d[1])-element" :
                       join(map(string,d), '×')

inds2string(inds::Indices) = join(map(string,inds), '×')

# anything array-like gets summarized e.g. 10-element Array{Int64,1}
summary(a::AbstractArray) = _summary(a, to_shape(indices(a)))
_summary(a, dims::Dims) = string(dims2string(dims), " ", typeof(a))
_summary(a, inds) = string(typeof(a), " with indices ", inds2string(inds))

# n-dimensional arrays
function show_nd(io::IO, a::AbstractArray, print_matrix, label_slices)
    limit::Bool = get(io, :limit, false)
    if isempty(a)
        return
    end
    tailinds = tail(tail(indices(a)))
    nd = ndims(a)-2
    for I in CartesianRange(tailinds)
        idxs = I.I
        if limit
            for i = 1:nd
                ii = idxs[i]
                ind = tailinds[i]
                if length(ind) > 10
                    if ii == ind[4] && all(d->idxs[d]==first(tailinds[d]),1:i-1)
                        for j=i+1:nd
                            szj = size(a,j+2)
                            indj = tailinds[j]
                            if szj>10 && first(indj)+2 < idxs[j] <= last(indj)-3
                                @goto skip
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        @goto skip
                    end
                    if ind[3] < ii <= ind[end-3]
                        @goto skip
                    end
                end
            end
        end
        if label_slices
            print(io, "[:, :, ")
            for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
            println(io, idxs[end], "] =")
        end
        slice = view(a, indices(a,1), indices(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == map(last,tailinds) ? "" : "\n\n")
        @label skip
    end
end

"""
`print_matrix_repr(io, X)` prints matrix X with opening and closing square brackets.
"""
function print_matrix_repr(io, X::AbstractArray)
    limit = get(io, :limit, false)::Bool
    compact, prefix = array_eltype_show_how(X)
    if compact && !haskey(io, :compact)
        io = IOContext(io, :compact => compact)
    end
    indr, indc = indices(X,1), indices(X,2)
    nr, nc = length(indr), length(indc)
    rdots, cdots = false, false
    rr1, rr2 = UnitRange{Int}(indr), 1:0
    cr1, cr2 = UnitRange{Int}(indc), 1:0
    if limit
        if nr > 4
            rr1, rr2 = rr1[1:2], rr1[nr-1:nr]
            rdots = true
        end
        if nc > 4
            cr1, cr2 = cr1[1:2], cr1[nc-1:nc]
            cdots = true
        end
    end
    print(io, prefix, "[")
    for rr in (rr1, rr2)
        for i in rr
            for cr in (cr1, cr2)
                for j in cr
                    j > first(cr) && print(io, " ")
                    if !isassigned(X,i,j)
                        print(io, undef_ref_str)
                    else
                        el = X[i,j]
                        show(io, el)
                    end
                end
                if last(cr) == last(indc)
                    i < last(indr) && print(io, "; ")
                elseif cdots
                    print(io, " \u2026 ")
                end
            end
        end
        last(rr) != nr && rdots && print(io, "\u2026 ; ")
    end
    print(io, "]")
end

show(io::IO, X::AbstractArray) = showarray(io, X, true)

repremptyarray{T}(io::IO, X::Array{T}) = print(io, "Array{$T}(", join(size(X),','), ')')
repremptyarray(io, X) = nothing # by default, we don't know this constructor

function showarray(io::IO, X::AbstractArray, repr::Bool = true; header = true)
    if repr && ndims(X) == 1
        return show_vector(io, X, "[", "]")
    end
    if !haskey(io, :compact)
        io = IOContext(io, compact=true)
    end
    if !repr && get(io, :limit, false) && eltype(X) === Method
        # override usual show method for Vector{Method}: don't abbreviate long lists
        io = IOContext(io, :limit => false)
    end
    (!repr && header) && print(io, summary(X))
    if !isempty(X)
        (!repr && header) && println(io, ":")
        if ndims(X) == 0
            if isassigned(X)
                return show(io, X[])
            else
                return print(io, undef_ref_str)
            end
        end
        if repr
            if ndims(X) <= 2
                print_matrix_repr(io, X)
            else
                show_nd(io, X, print_matrix_repr, false)
            end
        else
            punct = (" ", "  ", "")
            if ndims(X) <= 2
                print_matrix(io, X, punct...)
            else
                show_nd(io, X,
                        (io, slice) -> print_matrix(io, slice, punct...),
                        !repr)
            end
        end
    elseif repr
        repremptyarray(io, X)
    end
end

showall(x) = showall(STDOUT, x)
function showall(io::IO, x)
    if !get(io, :limit, false)
        show(io, x)
    else
        show(IOContext(io, :limit => false), x)
    end
end

showcompact(x) = showcompact(STDOUT, x)
function showcompact(io::IO, x)
    if get(io, :compact, false)
        show(io, x)
    else
        show(IOContext(io, :compact => true), x)
    end
end

# returns compact, prefix
function array_eltype_show_how(X)
    e = eltype(X)
    if print_without_params(e)
        str = string(unwrap_unionall(e).name) # Print "Array" rather than "Array{T,N}"
    else
        str = string(e)
    end
    # Types hard-coded here are those which are created by default for a given syntax
    isleaftype(e), (!isempty(X) && (e===Float64 || e===Int || e===Char) ? "" : str)
end

function show_vector(io::IO, v, opn, cls)
    compact, prefix = array_eltype_show_how(v)
    limited = get(io, :limit, false)
    if compact && !haskey(io, :compact)
        io = IOContext(io, :compact => compact)
    end
    print(io, prefix)
    if limited && _length(v) > 20
        inds = indices1(v)
        show_delim_array(io, v, opn, ",", "", false, inds[1], inds[1]+9)
        print(io, "  \u2026  ")
        show_delim_array(io, v, "", ",", cls, false, inds[end-9], inds[end])
    else
        show_delim_array(io, v, opn, ",", cls, false)
    end
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

print_bit_chunk(c::UInt64, l::Integer) = print_bit_chunk(STDOUT, c, l)
print_bit_chunk(c::UInt64) = print_bit_chunk(STDOUT, c)

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
bitshow(B::BitArray) = bitshow(STDOUT, B)

bitstring(B::BitArray) = sprint(bitshow, B)
