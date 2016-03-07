# This file is a part of Julia. License is MIT: http://julialang.org/license

show(x) = show(STDOUT::IO, x)
print(io::IO, s::Symbol) = (write(io,s); nothing)

immutable IOContext{IO_t <: IO} <: AbstractPipe
    io::IO_t
    dict::ImmutableDict{Symbol, Any}
    function IOContext(io::IO_t, dict::ImmutableDict{Symbol, Any})
        assert(!(IO_t <: IOContext))
        return new(io, dict)
    end
end

"""
    IOContext{<:IO} <: IO

IOContext provides a mechanism for passing output-configuration keyword arguments through arbitrary show methods.

In short, it is an immutable Dictionary that is a subclass of IO.

    IOContext(io::IO, KV::Pair)

Create a new entry in the IO Dictionary for the key => value pair

 - use `(key => value) in dict` to see if this particular combination is in the properties set
 - use `get(dict, key, default)` to retrieve the most recent value for a particular key

    IOContext(io::IO, context::IOContext)

Create a IOContext that wraps an alternate IO but inherits the keyword arguments from the context
"""
IOContext

IOContext(io::IOContext) = io
IOContext(io::IO) = IOContext(io, ImmutableDict{Symbol,Any}())

IOContext(io::IOContext, dict::ImmutableDict) = typeof(io)(io.io, dict)
IOContext(io::IO, dict::ImmutableDict) = IOContext{typeof(io)}(io, dict)

IOContext(io::IO, key, value) = IOContext(io, ImmutableDict{Symbol, Any}(key, value))
IOContext(io::IOContext, key, value) = IOContext(io, ImmutableDict{Symbol, Any}(io.dict, key, value))

IOContext(io::IO, context::IO) = IOContext(io)
IOContext(io::IO, context::IOContext) = IOContext(io, context.dict)
IOContext(io::IO, KV::Pair) = IOContext(io, KV[1], KV[2])

show(io::IO, ctx::IOContext) = (print(io, "IOContext("); show(io, ctx.io); print(io, ")"))

pipe_reader(io::IOContext) = io.io
pipe_writer(io::IOContext) = io.io
lock(io::IOContext) = lock(io.io)
unlock(io::IOContext) = unlock(io.io)

in(key_value::Pair, io::IOContext) = in(key_value, io.dict, is)
in(key_value::Pair, io::IO) = false
haskey(io::IOContext, key) = haskey(io.dict, key)
haskey(io::IO, key) = false
getindex(io::IOContext, key) = getindex(io.dict, key)
getindex(io::IO, key) = throw(KeyError(key))
get(io::IOContext, key, default) = get(io.dict, key, default)
get(io::IO, key, default) = default

"    limit_output(io) -> Bool
Output hinting for identifying contexts where the user requested a compact output"
limit_output(::ANY) = _limit_output::Bool
limit_output(io::IOContext) = get(io, :limit_output, _limit_output::Bool) === true
_limit_output = false # delete with with_output_limit deprecation

displaysize(io::IOContext) = haskey(io, :displaysize) ? io[:displaysize] : displaysize(io.io)


show(io::IO, x::ANY) = show_default(io, x)
function show_default(io::IO, x::ANY)
    t = typeof(x)::DataType
    show(io, t)
    print(io, '(')
    nf = nfields(t)
    if nf != 0 || t.size==0
        if (:SHOWN_SET => x) in io
            print(io, "#= circular reference =#")
        else
            recur_io = IOContext(io, :SHOWN_SET => x)
            for i=1:nf
                f = fieldname(t, i)
                if !isdefined(x, f)
                    print(io, undef_ref_str)
                else
                    show(recur_io, x.(f))
                end
                if i < nf
                    print(io, ',')
                end
            end
        end
    else
        nb = t.size
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
    if (mod === Base || mod === Core) && isexported(mod, name)
        return true
    end
    parent = module_parent(mod)
    if parent !== mod && isdefined(mod, name) && isdefined(parent, name) &&
       getfield(mod, name) === getfield(parent, name)
        return is_exported_from_stdlib(name, parent)
    end
    return false
end

function show(io::IO, f::Function)
    mt = typeof(f).name.mt
    if !isdefined(mt, :module) || is_exported_from_stdlib(mt.name, mt.module) || mt.module === Main
        print(io, mt.name)
    else
        print(io, mt.module, ".", mt.name)
    end
end

function show(io::IO, x::IntrinsicFunction)
    print(io, "(intrinsic function #", box(Int32,unbox(IntrinsicFunction,x)), ")")
end

function show(io::IO, x::Union)
    print(io, "Union")
    sorted_types = sort!(collect(x.types); by=string)
    show_comma_array(io, sorted_types, '{', '}')
end

show(io::IO, x::TypeConstructor) = show(io, x.body)

function show_type_parameter(io::IO, p::ANY)
    if p === ByteString
        print(io, "ByteString")
    else
        show(io, p)
    end
end

function show(io::IO, x::DataType)
    show(io, x.name)
    if (!isempty(x.parameters) || x.name === Tuple.name) && x !== Tuple
        print(io, '{')
        n = length(x.parameters)
        for (i, p) in enumerate(x.parameters)
            show_type_parameter(io, p)
            i < n && print(io, ',')
        end
        print(io, '}')
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

show{T}(io::IO, p::Ptr{T}) = print(io, typeof(p), " @0x$(hex(UInt(p), WORD_SIZE>>2))")

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
end

function show(io::IO, m::Module)
    if is(m,Main)
        print(io, "Main")
    else
        print(io, join(fullname(m),"."))
    end
end

function show(io::IO, l::LambdaInfo)
    print(io, "AST(")
    show(io, uncompressed_ast(l))
    print(io, ")")
end

function show_delim_array(io::IO, itr::Union{AbstractArray,SimpleVector}, op, delim, cl, delim_one,
                          i1=1, l=length(itr))
    print(io, op)
    newline = true
    first = true
    i = i1
    if l > 0
        while true
            if !isassigned(itr, i)
                print(io, undef_ref_str)
                multiline = false
            else
                x = itr[i]
                multiline = isa(x,AbstractArray) && ndims(x)>1 && !isempty(x)
                newline && multiline && println(io)
                if !isbits(x) && is(x, itr)
                    print(io, "#= circular reference =#")
                else
                    showcompact_lim(io, x)
                end
            end
            i += 1
            if i > i1+l-1
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
    print(io, cl)
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one, i1=1, n=typemax(Int))
    print(io, op)
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
            if !isbits(x) && is(x, itr)
                print(io, "#= circular reference =#")
            else
                show(io, x)
            end
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
                         LabelNode, GotoNode, TopNode, GlobalRef}
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
const expr_infix_wide = Set{Symbol}([:(=), :(+=), :(-=), :(*=), :(/=), :(\=), :(&=),
    :(|=), :($=), :(>>>=), :(>>=), :(<<=), :(&&), :(||), :(<:), :(=>), :(÷=)])
const expr_infix = Set{Symbol}([:(:), :(->), symbol("::")])
const expr_infix_any = union(expr_infix, expr_infix_wide)
const all_ops = union(quoted_syms, uni_ops, expr_infix_any)
const expr_calls  = Dict(:call =>('(',')'), :calldecl =>('(',')'), :ref =>('[',']'), :curly =>('{','}'))
const expr_parens = Dict(:tuple=>('(',')'), :vcat=>('[',']'), :cell1d=>("Any[","]"),
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

function is_intrinsic_expr(x::ANY)
    isa(x, IntrinsicFunction) && return true
    if isa(x, GlobalRef)
        x = x::GlobalRef
        return (x.mod == Base && isdefined(Base, x.name) &&
                isa(getfield(Base, x.name), IntrinsicFunction))
    elseif isa(x, TopNode)
        x = x::TopNode
        return (isdefined(Base, x.name) &&
                isa(getfield(Base, x.name), IntrinsicFunction))
    end
    return false
end

## AST printing helpers ##

typeemphasize(io::IO) = get(io, :TYPEEMPHASIZE, false) === true

const indent_width = 4

function show_expr_type(io::IO, ty, emph)
    if is(ty, Function)
        print(io, "::F")
    elseif is(ty, IntrinsicFunction)
        print(io, "::I")
    else
        if emph && (!isleaftype(ty) || ty == Box)
            emphasize(io, "::$ty")
        else
            print(io, "::$ty")
        end
    end
end

emphasize(io, str::AbstractString) = have_color ? print_with_color(:red, io, str) : print(io, uppercase(str))

show_linenumber(io::IO, line)       = print(io," # line ",line,':')
show_linenumber(io::IO, line, file) = print(io," # ", file,", line ",line,':')

# show a block, e g if/for/etc
function show_block(io::IO, head, args::Vector, body, indent::Int)
    print(io, head, ' ')
    show_list(io, args, ", ", indent)

    ind = is(head, :module) || is(head, :baremodule) ? indent : indent + indent_width
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
show_unquoted(io::IO, ex::LineNumberNode, ::Int, ::Int) = show_linenumber(io, ex.line, ex.file)
show_unquoted(io::IO, ex::LabelNode, ::Int, ::Int)      = print(io, ex.label, ": ")
show_unquoted(io::IO, ex::GotoNode, ::Int, ::Int)       = print(io, "goto ", ex.label)
show_unquoted(io::IO, ex::TopNode, ::Int, ::Int)        = print(io,"top(",ex.name,')')
show_unquoted(io::IO, ex::GlobalRef, ::Int, ::Int)      = print(io, ex.mod, '.', ex.name)

function show_unquoted(io::IO, ex::Slot, ::Int, ::Int)
    print(io, "_", ex.id)
    emphstate = typeemphasize(io)
    if emphstate || ex.typ !== Any
        show_expr_type(io, ex.typ, emphstate)
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
            print(io, "symbol(\"", escape_string(s), "\")")
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

# TODO: implement interpolated strings
function show_unquoted(io::IO, ex::Expr, indent::Int, prec::Int)
    head, args, nargs = ex.head, ex.args, length(ex.args)
    emphstate = typeemphasize(io)
    show_type = true
    if (ex.head == :(=) ||
        ex.head == :boundscheck ||
        ex.head == :gotoifnot ||
        ex.head == :return)
        show_type = false
    end
    if !emphstate && ex.typ === Any
        show_type = false
    end
    # dot (i.e. "x.y")
    if is(head, :(.))
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
    elseif (head in expr_infix_any && nargs==2) || (is(head,:(:)) && nargs==3)
        func_prec = operator_precedence(head)
        head_ = head in expr_infix_wide ? " $head " : head
        if func_prec <= prec
            show_enclosed_list(io, '(', args, head_, ')', indent, func_prec, true)
        else
            show_list(io, args, head_, indent, func_prec, true)
        end

    # list (i.e. "(1,2,3)" or "[1,2,3]")
    elseif haskey(expr_parens, head)               # :tuple/:vcat/:cell1d
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

        if (in(ex.args[1], (GlobalRef(Base, :box), TopNode(:box), :throw)) ||
            ismodulecall(ex) ||
            (ex.typ === Any && is_intrinsic_expr(ex.args[1])))
            show_type = false
        end
        if show_type
            prec = prec_decl
        end

        # scalar multiplication (i.e. "100x")
        if (func == :(*) &&
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
            if na == 2 || (na > 2 && func in (:+, :++, :*))
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

    # other call-like expressions ("A[1,2]", "T{X,Y}")
    elseif haskey(expr_calls, head) && nargs >= 1  # :ref/:curly/:calldecl
        show_call(io, head, ex.args[1], ex.args[2:end], indent)

    # comprehensions
    elseif (head === :typed_comprehension || head === :typed_dict_comprehension) && length(args) == 3
        isdict = (head === :typed_dict_comprehension)
        isdict && print(io, '(')
        show_unquoted(io, args[1], indent)
        isdict && print(io, ')')
        print(io, '[')
        show_unquoted(io, args[2], indent)
        print(io, " for ")
        show_unquoted(io, args[3], indent)
        print(io, ']')

    elseif (head === :comprehension || head === :dict_comprehension) && length(args) == 2
        print(io, '[')
        show_unquoted(io, args[1], indent)
        print(io, " for ")
        show_unquoted(io, args[2], indent)
        print(io, ']')

    elseif is(head, :ccall)
        show_unquoted(io, :ccall, indent)
        show_enclosed_list(io, '(', args, ",", ')', indent)

    # comparison (i.e. "x < y < z")
    elseif is(head, :comparison) && nargs >= 3 && (nargs&1==1)
        comp_prec = minimum(operator_precedence, args[2:2:end])
        if comp_prec <= prec
            show_enclosed_list(io, '(', args, " ", ')', indent, comp_prec)
        else
            show_list(io, args, " ", indent, comp_prec)
        end

    # function calls need to transform the function from :call to :calldecl
    # so that operators are printed correctly
    elseif head == :function && nargs==2 && is_expr(args[1], :call)
        show_block(io, head, Expr(:calldecl, args[1].args...), args[2], indent)
        print(io, "end")

    # block with argument
    elseif head in (:for,:while,:function,:if) && nargs==2
        show_block(io, head, args[1], args[2], indent)
        print(io, "end")

    elseif is(head, :module) && nargs==3 && isa(args[1],Bool)
        show_block(io, args[1] ? :module : :baremodule, args[2], args[3], indent)
        print(io, "end")

    # type declaration
    elseif is(head, :type) && nargs==3
        show_block(io, args[1] ? :type : :immutable, args[2], args[3], indent)
        print(io, "end")

    elseif is(head, :bitstype) && nargs == 2
        print(io, "bitstype ")
        show_list(io, args, ' ', indent)

    # empty return (i.e. "function f() return end")
    elseif is(head, :return) && nargs == 1 && is(args[1], nothing)
        print(io, head)

    # type annotation (i.e. "::Int")
    elseif is(head, symbol("::")) && nargs == 1
        print(io, "::")
        show_unquoted(io, args[1], indent)

    # var-arg declaration or expansion
    # (i.e. "function f(L...) end" or "f(B...)")
    elseif is(head, :(...)) && nargs == 1
        show_unquoted(io, args[1], indent)
        print(io, "...")

    elseif (nargs == 1 && head in (:return, :abstract, :const)) ||
                          head in (:local,  :global, :export)
        print(io, head, ' ')
        show_list(io, args, ", ", indent)

    elseif is(head, :macrocall) && nargs >= 1
        # Use the functional syntax unless specifically designated with prec=-1
        if prec >= 0
            show_call(io, :call, ex.args[1], ex.args[2:end], indent)
        else
            show_list(io, args, ' ', indent)
        end

    elseif is(head, :typealias) && nargs == 2
        print(io, "typealias ")
        show_list(io, args, ' ', indent)

    elseif is(head, :line) && 1 <= nargs <= 2
        show_linenumber(io, args...)

    elseif is(head, :if) && nargs == 3     # if/else
        show_block(io, "if",   args[1], args[2], indent)
        show_block(io, "else", args[3], indent)
        print(io, "end")

    elseif is(head, :try) && 3 <= nargs <= 4
        show_block(io, "try", args[1], indent)
        if is_expr(args[3], :block)
            show_block(io, "catch", is(args[2], false) ? Any[] : args[2], args[3], indent)
        end
        if nargs >= 4 && is_expr(args[4], :block)
            show_block(io, "finally", Any[], args[4], indent)
        end
        print(io, "end")

    elseif is(head, :let) && nargs >= 1
        show_block(io, "let", args[2:end], args[1], indent); print(io, "end")

    elseif is(head, :block) || is(head, :body)
        show_block(io, "begin", ex, indent); print(io, "end")

    elseif is(head, :quote) && nargs == 1 && isa(args[1],Symbol)
        show_unquoted_quote_expr(io, args[1], indent, 0)

    elseif is(head, :gotoifnot) && nargs == 2
        print(io, "unless ")
        show_list(io, args, " goto ", indent)

    elseif is(head, :string) && nargs == 1 && isa(args[1], AbstractString)
        show(io, args[1])

    elseif is(head, :null)
        print(io, "nothing")

    elseif is(head, :kw) && length(args)==2
        show_unquoted(io, args[1], indent+indent_width)
        print(io, '=')
        show_unquoted(io, args[2], indent+indent_width)

    elseif is(head, :string)
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
                print_escaped(io, x, "\"\$")
            end
        end
        print(io, '"')

    elseif (is(head, :&)#= || is(head, :$)=#) && length(args) == 1
        print(io, head)
        a1 = args[1]
        parens = (isa(a1,Expr) && a1.head !== :tuple) || (isa(a1,Symbol) && isoperator(a1))
        parens && print(io, "(")
        show_unquoted(io, a1)
        parens && print(io, ")")

    # transpose
    elseif (head === symbol('\'') || head === symbol(".'")) && length(args) == 1
        if isa(args[1], Symbol)
            show_unquoted(io, args[1])
        else
            print(io, "(")
            show_unquoted(io, args[1])
            print(io, ")")
        end
        print(io, head)

    elseif is(head, :import) || is(head, :importall) || is(head, :using)
        print(io, head)
        first = true
        for a = args
            if first
                print(io, ' ')
                first = false
            else
                print(io, '.')
            end
            if !is(a, :.)
                print(io, a)
            end
        end

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
end

function ismodulecall(ex::Expr)
    ex.head == :call && ex.args[1] == TopNode(:getfield) &&
        isa(ex.args[2], Symbol) &&
        isdefined(current_module(), ex.args[2]) &&
        isa(getfield(current_module(), ex.args[2]), Module)
end

function show(io::IO, tv::TypeVar)
    tvar_env = isa(io, IOContext) && get(io, :tvar_env, false)
    if isa(tvar_env, Vector{Any})
        have_env = true
        in_env = (tv in tvar_env::Vector{Any})
    else
        have_env = false
        in_env = true
    end
    if !in_env && !is(tv.lb, Bottom)
        show(io, tv.lb)
        print(io, "<:")
    end
    write(io, tv.name)
    if have_env ? !in_env : !is(tv.ub, Any)
        print(io, "<:")
        show(io, tv.ub)
    end
end

# dump & xdump - structured tree representation like R's str()
# - dump is for the user-facing structure
# - xdump is for the internal structure
#
# x is the object
# n is the depth of traversal in nested types (5 is the default)
# indent is a character string of spaces that is incremented at
# each descent.
#
# Package writers may overload dump for other nested types like lists
# or DataFrames. If overloaded, check the nesting level (n), and if
# n > 0, dump each component. Limit to the first 10 entries. When
# dumping components, decrement n, and add two spaces to indent.
#
# Package writers should not overload xdump.

function xdump(fn::Function, io::IO, x, n::Int, indent)
    T = typeof(x)
    print(io, T, " ")
    if isa(T, DataType) && nfields(T) > 0
        println(io)
        if n > 0
            for field in fieldnames(T)
                if field != symbol("")    # prevents segfault if symbol is blank
                    print(io, indent, "  ", field, ": ")
                    if isdefined(x,field)
                        fn(io, getfield(x, field), n - 1, string(indent, "  "))
                    else
                        println(io, undef_ref_str)
                    end
                end
            end
        end
    else
        println(io, x)
    end
end
function xdump(fn::Function, io::IO, x::Module, n::Int, indent)
    print(io, Module, " ")
    println(io, x)
end
function xdump_elts(fn::Function, io::IO, x::Array{Any}, n::Int, indent, i0, i1)
    for i in i0:i1
        print(io, indent, "  ", i, ": ")
        if !isdefined(x,i)
            println(io, undef_ref_str)
        else
            fn(io, x[i], n - 1, string(indent, "  "))
        end
    end
end
function xdump(fn::Function, io::IO, x::Array{Any}, n::Int, indent)
    println(io, "Array($(eltype(x)),$(size(x)))")
    if n > 0
        xdump_elts(fn, io, x, n, indent, 1, (length(x) <= 10 ? length(x) : 5))
        if length(x) > 10
            println(io, indent, "  ...")
            xdump_elts(fn, io, x, n, indent, length(x)-4, length(x))
        end
    end
end
xdump(fn::Function, io::IO, x::Symbol, n::Int, indent) = println(io, typeof(x), " ", x)
xdump(fn::Function, io::IO, x::Function, n::Int, indent) = println(io, x)
xdump(fn::Function, io::IO, x::Array, n::Int, indent) =
               (print(io, "Array($(eltype(x)),$(size(x))) ");
                show(io, x); println(io))

# Types
xdump(fn::Function, io::IO, x::Union, n::Int, indent) = println(io, x)
function xdump(fn::Function, io::IO, x::DataType, n::Int, indent)
    println(io, x, "::", typeof(x), " ", " <: ", supertype(x))
    fields = fieldnames(x)
    if n > 0
        for idx in 1:min(10, length(fields))
            if fields[idx] != symbol("")    # prevents segfault if symbol is blank
                print(io, indent, "  ", fields[idx], "::")
                if isa(x.types[idx], DataType)
                    xdump(fn, io, fieldtype(x,idx), n - 1, string(indent, "  "))
                else
                    println(io, fieldtype(x,idx))
                end
            end
        end
        if length(fields) > 10
            println(io, indent, "  ...")
        end
    end
end

# dumptype is for displaying abstract type hierarchies like Jameson
# Nash's wiki page: https://github.com/JuliaLang/julia/wiki/Types-Hierarchy

function dumptype(io::IO, x, n::Int, indent)
    # based on Jameson Nash's examples/typetree.jl
    println(io, x)
    if n == 0   # too deeply nested
        return
    end
    typargs(t) = split(string(t), "{")[1]
    # todo: include current module?
    for m in (Core, Base)
        for s in fieldnames(m)
            if isdefined(m,s)
                t = eval(m,s)
                if isa(t, TypeConstructor)
                    if string(x.name) == typargs(t) ||
                        ("Union" == split(string(t), "(")[1] &&
                         any(map(tt -> string(x.name) == typargs(tt), t.body.types)))
                        targs = join(t.parameters, ",")
                        println(io, indent, "  ", s,
                                !isempty(t.parameters) ? "{$targs}" : "",
                                " = ", t)
                    end
                elseif isa(t, Union)
                    if any(tt -> string(x.name) == typargs(tt), t.types)
                        println(io, indent, "  ", s, " = ", t)
                    end
                elseif isa(t, DataType) && supertype(t).name == x.name
                    # type aliases
                    if string(s) != string(t.name)
                        println(io, indent, "  ", s, " = ", t.name)
                    elseif t != Any
                        print(io, indent, "  ")
                        dump(io, t, n - 1, string(indent, "  "))
                    end
                end
            end
        end
    end
end

# For abstract types, use _dumptype only if it's a form that will be called
# interactively.
xdump(fn::Function, io::IO, x::DataType) = x.abstract ? dumptype(io, x, 5, "") : xdump(fn, io, x, 5, "")
xdump(fn::Function, io::IO, x::DataType, n::Int) = x.abstract ? dumptype(io, x, n, "") : xdump(fn, io, x, n, "")

# defaults:
xdump(fn::Function, io::IO, x) = xdump(xdump, io, x, 5, "")  # default is 5 levels
xdump(fn::Function, io::IO, x, n::Int) = xdump(xdump, io, x, n, "")
xdump(fn::Function, io::IO, args...) = throw(ArgumentError("invalid arguments to xdump"))
xdump(fn::Function, args...) = xdump(fn, STDOUT::IO, args...)
xdump(io::IO, args...) = xdump(xdump, io, args...)
xdump(args...) = xdump(xdump, IOContext(STDOUT::IO, :limit_output => true), args...)
xdump(arg::IO) = xdump(xdump, STDOUT::IO, arg)

# Here are methods specifically for dump:
dump(io::IO, x, n::Int) = dump(io, x, n, "")
dump(io::IO, x) = dump(io, x, 5, "")  # default is 5 levels
dump(io::IO, x::AbstractString, n::Int, indent) =
               (print(io, typeof(x), " ");
                show(io, x); println(io))
dump(io::IO, x, n::Int, indent) = xdump(dump, io, x, n, indent)
dump(io::IO, args...) = throw(ArgumentError("invalid arguments to dump"))
dump(arg::IO) = xdump(dump, STDOUT::IO, arg)
dump(args...) = dump(IOContext(STDOUT::IO, :limit_output => true), args...)

function dump(io::IO, x::Dict, n::Int, indent)
    println(io, typeof(x), " len ", length(x))
    if n > 0
        i = 1
        for (k,v) in x
            print(io, indent, "  ", k, ": ")
            dump(io, v, n - 1, string(indent, "  "))
            if i > 10
                println(io, indent, "  ...")
                break
            end
            i += 1
        end
    end
end

# More generic representation for common types:
dump(io::IO, x::DataType, n::Int, indent) = println(io, x.name)
dump(io::IO, x::DataType, n::Int) = dump(io, x, n, "")
dump(io::IO, x::DataType) = dump(io, x, 5, "")
dump(io::IO, x::TypeVar, n::Int, indent) = println(io, x.name)


"""
`alignment(X)` returns a tuple (left,right) showing how many characters are
needed on either side of an alignment feature such as a decimal point.
"""
alignment(io::IO, x::Any) = (0, length(sprint(0, showcompact_lim, x, env=io)))
alignment(io::IO, x::Number) = (length(sprint(0, showcompact_lim, x, env=io)), 0)
"`alignment(42)` yields (2,0)"
alignment(io::IO, x::Integer) = (length(sprint(0, showcompact_lim, x, env=io)), 0)
"`alignment(4.23)` yields (1,3) for `4` and `.23`"
function alignment(io::IO, x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(0, showcompact_lim, x, env=io))
    m === nothing ? (length(sprint(0, showcompact_lim, x, env=io)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
"`alignment(1 + 10im)` yields (3,5) for `1 +` and `_10im` (plus sign on left, space on right)"
function alignment(io::IO, x::Complex)
    m = match(r"^(.*[\+\-])(.*)$", sprint(0, showcompact_lim, x, env=io))
    m === nothing ? (length(sprint(0, showcompact_lim, x, env=io)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(io::IO, x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(0, showcompact_lim, x, env=io))
    m === nothing ? (length(sprint(0, showcompact_lim, x, env=io)), 0) :
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
function alignment(
    io::IO, X::AbstractVecOrMat,
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer
)
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
    if 1 < length(a) < size(X,2)
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
    return join(setindex!([utf8(" ") for i=1:N],string(c),ceil(Int,N/2)))
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
    for k = 1:length(A)
        j = cols[k]
        if isassigned(X,Int(i),Int(j)) # isassigned accepts only `Int` indices
            x = X[i,j]
            a = alignment(io, x)
            sx = sprint(0, showcompact_lim, x, env=io)
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
function print_matrix_vdots(io::IO,
    vdots::AbstractString, A::Vector, sep::AbstractString, M::Integer, m::Integer
)
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
`print_matrix(io, X)` composes an entire matrix, taking into account the screen size.
If X is too big, it will be nine-sliced with vertical, horizontal, or diagonal
ellipsis inserted as appropriate.
Optional parameters are screen size tuple sz such as (24,80),
string pre prior to the matrix (e.g. opening bracket), which will cause
a corresponding same-size indent on following rows,
string post on the end of the last row of the matrix.
Also options to use different ellipsis characters hdots,
vdots, ddots. These are repeated every hmod or vmod elements.
"""
function print_matrix(io::IO, X::AbstractVecOrMat,
                      pre::AbstractString = " ",  # pre-matrix string
                      sep::AbstractString = "  ", # separator between elements
                      post::AbstractString = "",  # post-matrix string
                      hdots::AbstractString = "  \u2026  ",
                      vdots::AbstractString = "\u22ee",
                      ddots::AbstractString = "  \u22f1  ",
                      hmod::Integer = 5, vmod::Integer = 5)
    if !limit_output(io)
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
    m, n = size(X,1), size(X,2)
    # To figure out alignments, only need to look at as many rows as could
    # fit down screen. If screen has at least as many rows as A, look at A.
    # If not, then we only need to look at the first and last chunks of A,
    # each half a screen height in size.
    halfheight = div(screenheight,2)
    rowsA = m <= screenheight ? (1:m) : [1:halfheight; m-div(screenheight-1,2)+1:m]
    # Similarly for columns, only necessary to get alignments for as many
    # columns as could conceivably fit across the screen
    maxpossiblecols = div(screenwidth, 1+sepsize)
    colsA = n <= maxpossiblecols ? (1:n) : [1:maxpossiblecols; (n-maxpossiblecols+1):n]
    A = alignment(io, X, rowsA, colsA, screenwidth, screenwidth, sepsize)
    # Nine-slicing is accomplished using print_matrix_row repeatedly
    if m <= screenheight # rows fit vertically on screen
        if n <= length(A) # rows and cols fit so just print whole matrix in one piece
            for i in rowsA
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        else # rows fit down screen but cols don't, so need horizontal ellipsis
            c = div(screenwidth-length(hdots)+1,2)+1  # what goes to right of ellipsis
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize)) # alignments for right
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize) # alignments for left of ellipsis
            for i in rowsA
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,Lalign,i,1:length(Lalign),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,Ralign,i,n-length(Ralign)+colsA,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        end
    else # rows don't fit so will need vertical ellipsis
        if n <= length(A) # rows don't fit, cols do, so only vertical ellipsis
            for i in rowsA
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,colsA,sep)
                print(io, i == m ? post : postsp)
                if i != rowsA[end]; println(io, ); end
                if i == halfheight
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,A,sep,vmod,1)
                    println(io, i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit, so use all 3 kinds of dots
            c = div(screenwidth-length(hdots)+1,2)+1
            Ralign = reverse(alignment(io, X, rowsA, reverse(colsA), c, c, sepsize))
            c = screenwidth - sum(map(sum,Ralign)) - (length(Ralign)-1)*sepsize - length(hdots)
            Lalign = alignment(io, X, rowsA, colsA, c, c, sepsize)
            r = mod((length(Ralign)-n+1),vmod) # where to put dots on right half
            for i in rowsA
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,Lalign,i,1:length(Lalign),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,Ralign,i,n-length(Ralign)+colsA,sep)
                print(io, i == m ? post : postsp)
                if i != rowsA[end]; println(io, ); end
                if i == halfheight
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,Lalign,sep,vmod,1)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots,Ralign,sep,vmod,r)
                    println(io, i == m ? post : postsp)
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
"""
summary(x) = string(typeof(x)) # e.g. Int64

# sizes such as 0-dimensional, 4-dimensional, 2x3
dims2string(d) = isempty(d) ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

# anything array-like gets summarized e.g. 10-element Array{Int64,1}
summary(a::AbstractArray) =
    string(dims2string(size(a)), " ", typeof(a))

# n-dimensional arrays
function show_nd(io::IO, a::AbstractArray, print_matrix, label_slices)
    limit::Bool = limit_output(io)
    if isempty(a)
        return
    end
    tail = size(a)[3:end]
    nd = ndims(a)-2
    for I in CartesianRange(tail)
        idxs = I.I
        if limit
            for i = 1:nd
                ii = idxs[i]
                if size(a,i+2) > 10
                    if ii == 4 && all(x->x==1,idxs[1:i-1])
                        for j=i+1:nd
                            szj = size(a,j+2)
                            if szj>10 && 3 < idxs[j] <= szj-3
                                @goto skip
                            end
                        end
                        #println(io, idxs)
                        print(io, "...\n\n")
                        @goto skip
                    end
                    if 3 < ii <= size(a,i+2)-3
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
        slice = sub(a, 1:size(a,1), 1:size(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == tail ? "" : "\n\n")
        @label skip
    end
end

"""
`print_matrix_repr(io, X)` prints matrix X with opening and closing square brackets.
"""
function print_matrix_repr(io, X::AbstractArray)
    compact, prefix = array_eltype_show_how(X)
    prefix *= "["
    ind = " "^length(prefix)
    print(io, prefix)
    for i=1:size(X,1)
        i > 1 && print(io, ind)
        for j=1:size(X,2)
            j > 1 && print(io, " ")
            if !isassigned(X,i,j)
                print(io, undef_ref_str)
            else
                el = X[i,j]
                compact ? showcompact_lim(io, el) : show(io, el)
            end
        end
        if i < size(X,1)
            println(io)
        else
            print(io, "]")
        end
    end
end

# NOTE: this is a possible, so-far-unexported function, providing control of
# array output. Not sure I want to do it this way.
showarray(X::AbstractArray; kw...) = showarray(STDOUT, X; kw...)
function showarray(io::IO, X::AbstractArray;
                   header::Bool=true, repr=false)
    header && print(io, summary(X))
    if !isempty(X)
        header && println(io, ":")
        if ndims(X) == 0
            if isassigned(X)
                return showcompact_lim(io, X[])
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
    end
end

show(io::IO, X::AbstractArray) = showarray(io, X, header=limit_output(io), repr=!limit_output(io))

showall(x) = showall(STDOUT, x)
function showall(io::IO, x)
    if !limit_output(io)
        show(io, x)
    else
        show(IOContext(io, :limit_output => false), x)
    end
end

# TODO: deprecated. remove this once methods for showcompact are gone
showcompact_lim(io, x) = limit_output(io) ? showcompact(io, x) : show(io, x)
showcompact_lim(io, x::Number) = limit_output(io) ? showcompact(io, x) : print(io, x)

showcompact(x) = showcompact(STDOUT, x)
function showcompact(io::IO, x)
    if limit_output(io)
        show(io, x)
    else
        show(IOContext(io, :limit_output => true), x)
    end
end

# returns compact, prefix
function array_eltype_show_how(X)
    e = eltype(X)
    leaf = isleaftype(e)
    plain = e<:Number || e<:AbstractString
    if isa(e,DataType) && e === e.name.primary
        str = string(e.name)
    else
        str = string(e)
    end
    leaf&&plain, (!isempty(X) && (e===Float64 || e===Int || (leaf && !plain)) ? "" : str)
end

function show_vector(io::IO, v, opn, cls)
    compact, prefix = array_eltype_show_how(v)
    limited = limit_output(io)
    if limited && !compact
        io = IOContext(io, :limit_output => false)
    end
    print(io, prefix)
    if limited && length(v) > 20
        show_delim_array(io, v, opn, ",", "", false, 1, 10)
        print(io, "  \u2026  ")
        n = length(v)
        show_delim_array(io, v, "", ",", cls, false, n-9, 10)
    else
        show_delim_array(io, v, opn, ",", cls, false)
    end
end

show(io::IO, v::AbstractVector) = show_vector(io, v, "[", "]")

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
