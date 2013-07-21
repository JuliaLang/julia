show(x) = show(STDOUT::IO, x)

function print(io::IO, s::Symbol)
    pname = convert(Ptr{Uint8}, s)
    write(io, pname, int(ccall(:strlen, Csize_t, (Ptr{Uint8},), pname)))
end

function show(io::IO, x::ANY)
    t = typeof(x)::DataType
    show(io, t)
    print(io, '(')
    if t.names !== () || t.size==0
        n = length(t.names)
        for i=1:n
            f = t.names[i]
            if !isdefined(x, f)
                print(io, undef_ref_str)
            else
                show(io, x.(f))
            end
            if i < n
                print(io, ',')
            end
        end
    else
        nb = t.size
        print(io, "0x")
        p = pointer_from_objref(x) + sizeof(Ptr{Void})
        for i=nb-1:-1:0
            print(io, hex(unsafe_load(convert(Ptr{Uint8}, p+i)), 2))
        end
    end
    print(io,')')
end

function show(io::IO, f::Function)
    if isgeneric(f)
        print(io, f.env.name)
    else
        print(io, "# function")
    end
end

function show(io::IO, x::IntrinsicFunction)
    print(io, "# intrinsic function ", box(Int32,unbox(IntrinsicFunction,x)))
end

function show(io::IO, x::UnionType)
    if is(x,None)
        print(io, "None")
    elseif is(x,Top)
        print(io, "Top")
    else
        print(io, "Union", x.types)
    end
end

show(io::IO, x::TypeConstructor) = show(io, x.body)

function show(io::IO, x::DataType)
    if isvarargtype(x)
        print(io, x.parameters[1], "...")
    else
        print(io, x.name.name)
        if length(x.parameters) > 0
            show_comma_array(io, x.parameters, '{', '}')
        end
    end
end

showcompact(io::IO, x) = show(io, x)
showcompact(x) = showcompact(STDOUT::IO, x)

macro show(exs...)
    blk = Expr(:block)
    for ex in exs
        push!(blk.args, :(println($(sprint(show_unquoted,ex)*" => "),
                                  repr(begin value=$(esc(ex)) end))))
    end
    if !isempty(exs); push!(blk.args, :value); end
    return blk
end

show(io::IO, s::Symbol) = show_indented(io, s)
show(io::IO, tn::TypeName) = print(io, tn.name)
show(io::IO, ::Nothing) = print(io, "nothing")
show(io::IO, b::Bool) = print(io, b ? "true" : "false")
show(io::IO, n::Signed) = (write(io, dec(n)); nothing)
show(io::IO, n::Unsigned) = print(io, "0x", hex(n,sizeof(n)<<1))

show{T}(io::IO, p::Ptr{T}) =
    print(io, is(T,None) ? "Ptr{Void}" : typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

function show(io::IO, m::Module)
    if is(m,Main)
        print(io, "Main")
    else
        print(io, join(fullname(m),"."))
    end
end

function show(io::IO, l::LambdaStaticData)
    print(io, "AST(")
    show(io, uncompressed_ast(l))
    print(io, ")")
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one)
    print(io, op)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
	while true
            if isa(itr,Array) && !isdefined(itr,state)
                print(io, undef_ref_str)
                state += 1
                multiline = false
            else
	        x, state = next(itr,state)
                multiline = isa(x,AbstractArray) && ndims(x)>1 && length(x)>0
                if newline
                    if multiline; println(io); end
                end
	        show(io, x)
            end
	    if done(itr,state)
                if delim_one && first
                    print(io, delim)
                end
		break
	    end
            first = false
            print(io, delim)
            if multiline
                println(io); println(io); newline=false
            else
                newline = true
            end
	end
    end
    print(io, cl)
end

show_comma_array(io::IO, itr, o, c) = show_delim_array(io, itr, o, ',', c, false)
show(io::IO, t::Tuple) = show_delim_array(io, t, '(', ',', ')', true)

## AST decoding helpers ##

is_expr(ex, head::Symbol)         = (isa(ex, Expr) && (ex.head == head))
is_expr(ex, head::Symbol, n::Int) = is_expr(ex, head) && length(ex.args) == n

is_linenumber(ex::LineNumberNode) = true
is_linenumber(ex::Expr)           = is(ex.head, :line)
is_linenumber(ex)                 = false

is_quoted(ex::QuoteNode) = true
is_quoted(ex::Expr)      = is_expr(ex, :quote, 1)
is_quoted(ex)            = false

unquoted(ex::QuoteNode)  = ex.value
unquoted(ex::Expr)       = ex.args[1]

## AST printing ##

# show(ex::Expr) delegates to show_indented(), 
# which shows the contents using show_unquoted(),
# which shows subexpressions using show_unquoted()
# ==> AST:s are printed wrapped in a single quotation
show_indented(x)                 = show_indented(STDOUT, x)
show_indented(io::IO, x)         = show_indented(io, x, 0)
show_indented(io::IO, x, indent) = show(io, x)
show_unquoted(x)                 = show_unquoted(STDOUT, x)
show_unquoted(io::IO, x)         = show_unquoted(io, x, 0)
show_unquoted(io::IO, x, indent) = (print(io,"\$("); show(io,x); print(io,')'))

## Quoted AST printing ##

typealias ExprNode Union(SymbolNode, LineNumberNode, LabelNode, GotoNode,
                         TopNode, QuoteNode)
show(io::IO, ex::ExprNode) = show_indented(io, ex)

function show_indented(io::IO, ex::ExprNode, indent::Int)
    default_show_quoted(io, ex, indent)
end
function show_indented(io::IO, ex::QuoteNode, indent::Int)
    show_indented(io, ex.value, indent)
end

show(io::IO, ex::Expr) = show_indented(io, ex)
function show_indented(io::IO, ex::Expr, indent::Int)
    if is(ex.head, :block) || is(ex.head, :body)
        show_block(io, "quote", ex, indent); print(io, "end")
    elseif contains((:tuple, :vcat, :cell1), ex.head)
        print(io, ':'); show_unquoted(io, ex, indent + indent_width)        
    else
        default_show_quoted(io, ex, indent)
    end
end
const paren_quoted_syms = Set{Symbol}(:(:),:(::),:(:=),:(=),:(==),:(===),:(=>))
function show_indented(io::IO, sym::Symbol, indent::Int)
    if contains(paren_quoted_syms, sym)
        print(io, ":($sym)")
    else
        print(io, ":$sym")
    end
end
function default_show_quoted(io::IO, ex, indent::Int)
    print(io, ":(")
    show_unquoted(io, ex, indent + indent_width)
    print(io, ")")
end

## AST printing helpers ##

const indent_width = 4

function show_expr_type(io::IO, ty)
    if !is(ty, Any)
        if is(ty, Function)
            print(io, "::F")
        elseif is(ty, IntrinsicFunction)
            print(io, "::I")
        else
            print(io, "::$ty")
        end
    end
end

show_linenumber(io::IO, line)       = print(io," # line ",line,':')
show_linenumber(io::IO, line, file) = print(io," # ",file,", line ",line,':')

# show a block, e g if/for/etc
function show_block(io::IO, head, args::Vector, body, indent::Int)
    print(io, head, ' ')
    show_list(io, args, ", ", indent)

    ind = is(head, :module) ? indent : indent + indent_width
    exs = (is_expr(body, :block) || is_expr(body, :body)) ? body.args : {body}
    for ex in exs
        if !is_linenumber(ex); print(io, '\n', " "^ind); end
        show_unquoted(io, ex, ind)
    end
    print(io, '\n', " "^indent)
end
show_block(io::IO,head,    block,i::Int) = show_block(io,head,{},   block,i)
show_block(io::IO,head,arg,block,i::Int) = show_block(io,head,{arg},block,i)

# show an indented list
function show_list(io::IO, items, sep, indent::Int)
    n = length(items)
    if n == 0; return end
    indent += indent_width
    show_unquoted(io, items[1], indent)
    for item in items[2:end]
        print(io, sep)
        show_unquoted(io, item, indent)        
    end
end
# show an indented list inside the parens (op, cl)
function show_enclosed_list(io::IO, op, items, sep, cl, indent)
    print(io, op); show_list(io, items, sep, indent); print(io, cl)
end

## Unquoted AST printing ##

show_unquoted(io::IO, sym::Symbol, indent::Int) = print(io, sym)
show_unquoted(io::IO, x::Number, indent::Int)   = show(io, x)
show_unquoted(io::IO, x::String, indent::Int)   = show(io, x)
show_unquoted(io::IO, x::Char, indent::Int)     = show(io, x)

const _expr_infix_wide = Set(:(=), :(+=), :(-=), :(*=), :(/=), :(\=), :(&=), 
    :(|=), :($=), :(>>>=), :(>>=), :(<<=), :(&&), :(||))
const _expr_infix = Set(:(:), :(<:), :(->), :(=>), symbol("::"))
const _expr_calls  = [:call =>('(',')'), :ref =>('[',']'), :curly =>('{','}')]
const _expr_parens = [:tuple=>('(',')'), :vcat=>('[',']'), :cell1d=>('{','}')]

function show_unquoted(io::IO, ex::Expr, indent::Int)
    head, args, nargs = ex.head, ex.args, length(ex.args)

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
    elseif (contains(_expr_infix, head) && nargs==2) || (is(head,:(:)) && nargs==3)
        show_list(io, args, head, indent)
    elseif contains(_expr_infix_wide, head) && nargs == 2
        show_list(io, args, " $head ", indent)
    elseif is(head, symbol("::")) && nargs == 1
        print(io, "::")        
        show_unquoted(io, args[1], indent)
    elseif haskey(_expr_parens, head)               # :tuple/:vcat/:cell1d
        op, cl = _expr_parens[head]
        print(io, op)
        show_list(io, args, ",", indent)
        if is(head, :tuple) && nargs == 1; print(io, ','); end
        print(io, cl)
    elseif haskey(_expr_calls, head) && nargs >= 1  # :call/:ref/:curly
        op, cl = _expr_calls[head]
        show_unquoted(io, args[1], indent)
        show_enclosed_list(io, op, args[2:end], ",", cl, indent)
    elseif is(head, :comparison) && nargs >= 3 && (nargs&1==1)
        show_enclosed_list(io, '(', args, "", ')', indent)
    elseif is(head, :(...)) && nargs == 1
        show_unquoted(io, args[1], indent)
        print(io, "...")
    elseif (nargs == 1 && contains((:return, :abstract, :const), head)) ||
                          contains((:local,  :global), head)
        print(io, head, ' ')
        show_list(io, args, ",", indent)
    elseif is(head, :macrocall) && nargs >= 1
        show_list(io, args, ' ', indent)
    elseif is(head, :typealias) && nargs == 2
        print(io, "typealias ")
        show_list(io, args, ' ', indent)
    elseif is(head, :line) && 1 <= nargs <= 2
        show_linenumber(io, args...)
    elseif is(head, :if) && nargs == 3           # if/else
        show_block(io, "if",   args[1], args[2], indent)
        show_block(io, "else", args[3], indent)
        print(io, "end")
    elseif is(head, :try) && nargs == 3
        show_block(io, "try", args[1], indent)
        if !(is(args[2], false) && is_expr(args[3], :block, 0))
            show_block(io, "catch", args[2], args[3], indent)
        end
        print(io, "end")
    elseif is(head, :let) && nargs >= 1
        show_block(io, "let", args[2:end], args[1], indent); print(io, "end")
    elseif is(head, :block) || is(head, :body)
        show_block(io, "begin", ex, indent); print(io, "end")
    elseif contains((:for,:while,:function,:if,:type,:module),head) && nargs==2
        show_block(io, head, args[1], args[2], indent); print(io, "end")
    elseif is(head, :quote) && nargs == 1
        show_indented(io, args[1], indent)
    elseif is(head, :gotoifnot) && nargs == 2
        print(io, "unless ")
        show_list(io, args, " goto ", indent)
    elseif is(head, :string) && nargs == 1 && isa(args[1], String)
        show(io, args[1])
    elseif is(head, :null)
        print(io, "nothing")
    else
        print(io, "\$(Expr(")
        show_indented(io, ex.head, indent)
        for arg in args
            print(io, ", ")
            show_indented(io, arg, indent)
        end
        print(io, "))")
    end
    show_expr_type(io, ex.typ)
end

show_unquoted(io::IO, ex::ExprNode, indent::Int) = show_unquoted(io, ex)

show_unquoted(io::IO, ex::LineNumberNode) = show_linenumber(io, ex.line)
show_unquoted(io::IO, ex::LabelNode)      = print(io, ex.label, ": ")
show_unquoted(io::IO, ex::GotoNode)       = print(io, "goto ", ex.label)
show_unquoted(io::IO, ex::TopNode)        = print(io, "top(", ex.name, ')')
show_unquoted(io::IO, ex::QuoteNode, ind::Int) = show_indented(io,ex.value,ind)
function show_unquoted(io::IO, ex::SymbolNode) 
    print(io, ex.name)
    show_expr_type(io, ex.typ)
end

function clean_gensym(s::Symbol)
    s = string(s)
    i = search(s,'#')
    if i > 0
        return s[1:i-1]
    end
    return s
end

function argtype_decl_string(n, t)
    if isa(n,Expr)
        n = n.args[1]  # handle n::T in arg list
    end
    n = clean_gensym(n)
    if t === Any && !isempty(n)
        return n
    end
    if t <: Vararg && t.parameters[1] === Any
        return string(n, "...")
    end
    return string(n, "::", t)
end

function show(io::IO, m::Method)
    tv = m.tvars
    if !isa(tv,Tuple)
        tv = (tv,)
    end
    if !isempty(tv)
        show_delim_array(io, tv, '{', ',', '}', false)
    end
    li = m.func.code
    e = uncompressed_ast(li)
    argnames = e.args[1]
    decls = map(argtype_decl_string, argnames, {m.sig...})
    print(io, "(")
    print_joined(io, decls, ",", ",")
    print(io, ")")
    if li.line > 0
        print(io, " at ", li.file, ":", li.line)
    end
end

function show_method_table(io::IO, mt::MethodTable, max::Int=-1)
    name = mt.name
    print(io, "# methods for generic function ", name)
    d = mt.defs
    n = rest = 0
    while !is(d,())
        if max==-1 || n<max || (rest==0 && n==max && d.next === ())
            println(io)
            print(io, name)
            show(io, d)
            n += 1
        else
            rest += 1
        end
        d = d.next
    end
    if rest > 0
        println(io)
        print(io,"... $rest methods not shown (use methods($name) to see them all)")
    end
end

show(io::IO, mt::MethodTable) = show_method_table(io, mt)

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
    if isa(T, DataType) && length(T.names) > 0
        println(io)
        if n > 0
            for field in T.names
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
            println(undef_ref_str)
        else
            fn(io, x[i], n - 1, string(indent, "  "))
        end
    end
end
function xdump(fn::Function, io::IO, x::Array{Any}, n::Int, indent)
    println("Array($(eltype(x)),$(size(x)))")
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
xdump(fn::Function, io::IO, x::Array, n::Int, indent) = println(io, "Array($(eltype(x)),$(size(x)))", " ", x)

# Types
xdump(fn::Function, io::IO, x::UnionType, n::Int, indent) = println(io, x)
function xdump(fn::Function, io::IO, x::DataType, n::Int, indent)
    println(io, x, "::", typeof(x), " ", " <: ", super(x))
    if n > 0
        for idx in 1:min(10,length(x.names))
            if x.names[idx] != symbol("")    # prevents segfault if symbol is blank
                print(io, indent, "  ", x.names[idx], "::")
                if isa(x.types[idx], DataType)
                    xdump(fn, io, x.types[idx], n - 1, string(indent, "  "))
                else
                    println(x.types[idx])
                end
            end
        end
        if length(x.names) > 10
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
        for s in names(m)
            if isdefined(m,s)
                t = eval(m,s)
                if isa(t, TypeConstructor)
                    if string(x.name) == typargs(t) ||
                        ("Union" == split(string(t), "(")[1] &&
                         any(map(tt -> string(x.name) == typargs(tt), t.body.types)))
                        targs = join(t.parameters, ",")
                        println(io, indent, "  ", s,
                                length(t.parameters) > 0 ? "{$targs}" : "",
                                " = ", t)
                    end
                elseif isa(t, UnionType)
                    if any(tt -> string(x.name) == typargs(tt), t.types)
                        println(io, indent, "  ", s, " = ", t)
                    end
                elseif isa(t, DataType) && super(t).name == x.name
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
xdump(fn::Function, args...) = xdump(fn, STDOUT::IO, args...)
xdump(io::IO, args...) = xdump(xdump, io, args...)
xdump(args...) = xdump(xdump, STDOUT::IO, args...)


# Here are methods specifically for dump:
dump(io::IO, x, n::Int) = dump(io, x, n, "")
dump(io::IO, x) = dump(io, x, 5, "")  # default is 5 levels
dump(args...) = dump(STDOUT::IO, args...)
dump(io::IO, x::String, n::Int, indent) = println(io, typeof(x), " \"", x, "\"")
dump(io::IO, x, n::Int, indent) = xdump(dump, io, x, n, indent)

function dump(io::IO, x::Dict, n::Int, indent)
    println(typeof(x), " len ", length(x))
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


showall(x) = showall(STDOUT::IO, x)

function showall{T}(io::IO, a::AbstractArray{T,1})
    if is(T,Any)
        opn = '{'; cls = '}'
    else
        opn = '['; cls = ']';
    end
    show_comma_array(io, a, opn, cls)
end

alignment(x::Any) = (0, length(sprint(showcompact, x)))
alignment(x::Number) = (length(sprint(showcompact, x)), 0)
alignment(x::Integer) = (length(sprint(showcompact, x)), 0)
function alignment(x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(showcompact, x))
    m == nothing ? (length(sprint(showcompact, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(x::Complex)
    m = match(r"^(.*,)(.*)$", sprint(showcompact, x))
    m == nothing ? (length(sprint(showcompact, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end
function alignment(x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(showcompact, x))
    m == nothing ? (length(sprint(showcompact, x)), 0) :
                   (length(m.captures[1]), length(m.captures[2]))
end

const undef_ref_str = "#undef"
const undef_ref_alignment = (3,3)

function alignment(
    X::Union(AbstractMatrix,AbstractVector),
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer
)
    a = {}
    for j in cols
        l = r = 0
        for i in rows
            if isassigned(X,i,j)
                aij = alignment(X[i,j])
            else
                aij = undef_ref_alignment
            end
            l = max(l, aij[1])
            r = max(r, aij[2])
        end
        push!(a, (l, r))
        if length(a) > 1 && sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop!(a)
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

function print_matrix_row(io::IO,
    X::Union(AbstractMatrix,AbstractVector), A::Vector,
    i::Integer, cols::AbstractVector, sep::String
)
    for k = 1:length(A)
        j = cols[k]
        if isassigned(X,i,j)
            x = X[i,j]
            a = alignment(x)
            sx = sprint(showcompact, x)
        else
            a = undef_ref_alignment
            sx = undef_ref_str
        end
        l = repeat(" ", A[k][1]-a[1])
        r = repeat(" ", A[k][2]-a[2])
        print(io, l, sx, r)
        if k < length(A); print(io, sep); end
    end
end

function print_matrix_vdots(io::IO,
    vdots::String, A::Vector, sep::String, M::Integer, m::Integer
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

function print_matrix(io::IO,
    X::Union(AbstractMatrix,AbstractVector), rows::Integer, cols::Integer,
    pre::String, sep::String, post::String,
    hdots::String, vdots::String, ddots::String,
    hmod::Integer, vmod::Integer
)
    cols -= length(pre) + length(post)
    presp = repeat(" ", length(pre))
    postsp = ""
    @assert strwidth(hdots) == strwidth(ddots)
    ss = length(sep)
    m, n = size(X,1), size(X,2)
    if m <= rows # rows fit
        A = alignment(X,1:m,1:n,cols,cols,ss)
        if n <= length(A) # rows and cols fit
            for i = 1:m
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,1:n,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        else # rows fit, cols don't
            c = div(cols-length(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - length(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                print(io, i == m ? post : postsp)
                if i != m; println(io, ); end
            end
        end
    else # rows don't fit
        t = div(rows,2)
        I = [1:t; m-div(rows-1,2)+1:m]
        A = alignment(X,I,1:n,cols,cols,ss)
        if n <= length(A) # rows don't fit, cols do
            for i in I
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,A,i,1:n,sep)
                print(io, i == m ? post : postsp)
                if i != I[end]; println(io, ); end
                if i == t
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,A,sep,vmod,1)
                    println(io, i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit
            c = div(cols-length(hdots)+1,2)+1
            R = reverse(alignment(X,I,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - length(hdots)
            L = alignment(X,I,1:n,c,c,ss)
            r = mod((length(R)-n+1),vmod)
            for i in I
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", length(hdots)))
                print_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                print(io, i == m ? post : postsp)
                if i != I[end]; println(io, ); end
                if i == t
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,L,sep,vmod,1)
                    print(io, ddots)
                    print_matrix_vdots(io, vdots,R,sep,vmod,r)
                    println(io, i == m ? post : postsp)
                end
            end
        end
    end
end
print_matrix(io::IO, X::Union(AbstractMatrix,AbstractVector),
             rows::Integer, cols::Integer) =
    print_matrix(io, X, rows, cols, " ", "  ", "",
                 "  \u2026  ", "\u22ee", "  \u22f1  ", 5, 5)

print_matrix(io::IO, X::Union(AbstractMatrix,AbstractVector)) =
               print_matrix(io, X, tty_rows()-4, tty_cols())

summary(x) = string(typeof(x))

dims2string(d) = length(d) == 0 ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

summary{T}(a::AbstractArray{T}) =
    string(dims2string(size(a)), " ", T, " ", typeof(a).name)

function show_nd(io::IO, a::AbstractArray)
    if isempty(a)
        return
    end
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(io::IO, idxs...)
        for i = 1:nd
            ii = idxs[i]
            if size(a,i+2) > 10
                if ii == 4 && all(x->x==1,idxs[1:i-1])
                    for j=i+1:nd
                        szj = size(a,j+2)
                        if szj>10 && 3 < idxs[j] <= szj-3
                            return
                        end
                    end
                    #println(io, idxs)
                    print(io, "...\n\n")
                    return
                end
                if 3 < ii <= size(a,i+2)-3
                    return
                end
            end
        end
        print(io, "[:, :, ")
        for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
        println(io, idxs[end], "] =")
        slice = sub(a, 1:size(a,1), 1:size(a,2), idxs...)
        print_matrix(io, slice)
        print(io, idxs == tail ? "" : "\n\n")
    end
    cartesianmap((idxs...)->print_slice(io,idxs...), tail)
end

function whos(m::Module, pattern::Regex)
    for v in sort(names(m))
        s = string(v)
        if isdefined(m,v) && ismatch(pattern, s)
            println(rpad(s, 30), summary(eval(m,v)))
        end
    end
end
whos() = whos(r"")
whos(m::Module) = whos(m, r"")
whos(pat::Regex) = whos(current_module(), pat)

function show{T}(io::IO, x::AbstractArray{T,0})
    println(io, summary(x),":")
    if isassigned(x)
        sx = sprint(showcompact, x[])
    else
        sx = undef_ref_str
    end
    print(io, sx)
end

function show(io::IO, X::AbstractArray)
    print(io, summary(X))
    if !isempty(X)
        println(io, ":")
        ndims(X)==2 ? print_matrix(io, X) : show_nd(io, X)
    end
end

function showall(io::IO, X::AbstractMatrix)
    print(io, summary(X))
    if !isempty(X)
        println(io, ":")
        print_matrix(io, X, typemax(Int64), typemax(Int64))
    end
end

function showall(io::IO, a::AbstractArray)
    print(io, summary(a))
    if isempty(a)
        return
    end
    println(io, ":")
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(io, idxs...)
        print(io, "[:, :, ")
        for i = 1:(nd-1); print(io, "$(idxs[i]), "); end
        println(io, idxs[end], "] =")
        slice = a[:,:,idxs...]
        print_matrix(io, reshape(slice, size(slice,1), size(slice,2)),
                     typemax(Int64), typemax(Int64))
        print(io, idxs == tail ? "" : "\n\n")
    end
    cartesianmap(print_slice, tail)
end

function show_vector(io::IO, v, opn, cls)
    show_delim_array(io, v, opn, ",", cls, false)
end

show(io::IO, v::AbstractVector{Any}) = show_vector(io, v, "{", "}")
show(io::IO, v::AbstractVector)      = show_vector(io, v, "[", "]")

# printing BitArrays

summary(a::BitArray) =
    string(dims2string(size(a)), " ", typeof(a).name)

# (following functions not exported - mainly intended for debug)

function print_bit_chunk(io::IO, c::Uint64, l::Integer)
    for s = 0 : l - 1
        d = (c >>> s) & 1
        print(io, "01"[d + 1])
        if (s + 1) & 7 == 0
            print(io, " ")
        end
    end
end

print_bit_chunk(io::IO, c::Uint64) = print_bit_chunk(io, c, 64)

print_bit_chunk(c::Uint64, l::Integer) = print_bit_chunk(STDOUT, c, l)
print_bit_chunk(c::Uint64) = print_bit_chunk(STDOUT, c)

function bitshow(io::IO, B::BitArray)
    if length(B) == 0
        return
    end
    for i = 1 : length(B.chunks) - 1
        print_bit_chunk(io, B.chunks[i])
        print(io, ": ")
    end
    l = (@_mod64 (length(B)-1)) + 1
    print_bit_chunk(io, B.chunks[end], l)
end
bitshow(B::BitArray) = bitshow(STDOUT, B)

bitstring(B::BitArray) = sprint(bitshow, B)
