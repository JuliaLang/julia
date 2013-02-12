# formerly built-in methods. can be replaced any time.

show(x) = show(OUTPUT_STREAM::IO, x)

print(io::IO, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void}, Any,), io, s)
show(io::IO, x::ANY) = ccall(:jl_show_any, Void, (Any, Any,), io::IO, x)

showcompact(io::IO, x) = show(io, x)
showcompact(x) = showcompact(OUTPUT_STREAM::IO, x)

macro show(exs...)
    blk = expr(:block)
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

full_name(m::Module) = m===Main ? () : tuple(full_name(module_parent(m))...,
                                             module_name(m))

function show(io::IO, m::Module)
    if is(m,Main)
        print(io, "Main")
    else
        print(io, join(full_name(m),"."))
    end
end

function show(io::IO, l::LambdaStaticData)
    print(io, "AST(")
    if isa(l.ast,Expr)
        show(io, l.ast)
    else
        ast = ccall(:jl_uncompress_ast, Any, (Any,Any), l, l.ast)
        show(io, ast)
    end
    print(io, ")")
end

function show_delim_array(io::IO, itr, op, delim, cl, delim_one)
    print(io, op)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
	while true
	    x, state = next(itr,state)
            multiline = isa(x,AbstractArray) && ndims(x)>1 && length(x)>0
            if newline
                if multiline; println(io); end
            end
	    show(io, x)
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
show_indented(x)                 = show_indented(OUTPUT_STREAM, x)
show_indented(io::IO, x)         = show_indented(io, x, 0)
show_indented(io::IO, x, indent) = show(io, x)
show_unquoted(x)                 = show_unquoted(OUTPUT_STREAM, x)
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
    if has(paren_quoted_syms, sym); print(io, ":($sym)")        
    else                            print(io, ":$sym")        
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
    elseif (has(_expr_infix, head) && nargs==2) || (is(head,:(:)) && nargs==3)
        show_list(io, args, head, indent)
    elseif has(_expr_infix_wide, head) && nargs == 2
        show_list(io, args, " $head ", indent)
    elseif is(head, symbol("::")) && nargs == 1
        print(io, "::")        
        show_unquoted(io, args[1], indent)
    elseif has(_expr_parens, head)               # :tuple/:vcat/:cell1d
        op, cl = _expr_parens[head]
        print(io, op)
        show_list(io, args, ",", indent)
        if is(head, :tuple) && nargs == 1; print(io, ','); end
        print(io, cl)
    elseif has(_expr_calls, head) && nargs >= 1  # :call/:ref/:curly
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
        print(io, "\$(expr(")
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

function show(io::IO, m::Method)
    tv = m.tvars
    if !isa(tv,Tuple)
        tv = (tv,)
    end
    if !isempty(tv)
        show_delim_array(io, tv, '{', ',', '}', false)
    end
    show(io, m.sig)
    li = m.func.code
    if li.line > 0
        print(io, " at ", li.file, ":", li.line)
    end
end

function show(io::IO, mt::MethodTable)
    name = mt.name
    println(io, "# methods for generic function ", name)
    d = mt.defs
    while !is(d,())
        print(io, name)
        show(io, d)
        d = d.next
        if !is(d,())
            println(io)
        end
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
    if isa(T, CompositeKind)
        println(io)
        if n > 0
            for field in T.names
                if field != symbol("")    # prevents segfault if symbol is blank
                    print(io, indent, "  ", field, ": ")
                    fn(io, getfield(x, field), n - 1, string(indent, "  "))
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
function xdump(fn::Function, io::IO, x::Array{Any}, n::Int, indent)
    println("Array($(eltype(x)),$(size(x)))")
    if n > 0
        for i in 1:(length(x) <= 10 ? length(x) : 5)
            print(io, indent, "  ", i, ": ")
            fn(io, x[i], n - 1, string(indent, "  "))
        end
        if length(x) > 10
            println(io, indent, "  ...")
            for i in length(x)-4:length(x)
                print(io, indent, "  ", i, ": ")
                fn(io, x[i], n - 1, string(indent, "  "))
            end
        end
    end
end
xdump(fn::Function, io::IO, x::Symbol, n::Int, indent) = println(io, typeof(x), " ", x)
xdump(fn::Function, io::IO, x::Function, n::Int, indent) = println(io, x)
xdump(fn::Function, io::IO, x::Array, n::Int, indent) = println(io, "Array($(eltype(x)),$(size(x)))", " ", x)

# Types
xdump(fn::Function, io::IO, x::UnionKind, n::Int, indent) = println(io, x)
function xdump(fn::Function, io::IO, x::CompositeKind, n::Int, indent)
    println(io, x, "::", typeof(x), " ", " <: ", super(x))
    if n > 0
        for idx in 1:min(10,length(x.names))
            if x.names[idx] != symbol("")    # prevents segfault if symbol is blank
                print(io, indent, "  ", x.names[idx], "::")
                if isa(x.types[idx], CompositeKind) 
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
                elseif isa(t, UnionKind)
                    if any(map(tt -> string(x.name) == typargs(tt), t.types))
                        println(io, indent, "  ", s, " = ", t)
                    end
                elseif isa(t, Type) && super(t).name == x.name
                    # type aliases
                    if string(s) != string(t.name)
                        println(io, indent, "  ", s, " = ", t.name)
                    elseif t != Any 
                        print(io, indent, "  ")
                        dumptype(io, t, n - 1, string(indent, "  "))
                    end
                end
            end
        end
    end
end

# For abstract types, use _dumptype only if it's a form that will be called
# interactively.
xdump(fn::Function, io::IO, x::AbstractKind) = dumptype(io, x, 5, "")
xdump(fn::Function, io::IO, x::AbstractKind, n::Int) = dumptype(io, x, n, "")

# defaults:
xdump(fn::Function, io::IO, x) = xdump(xdump, io, x, 5, "")  # default is 5 levels
xdump(fn::Function, io::IO, x, n::Int) = xdump(xdump, io, x, n, "")
xdump(fn::Function, args...) = xdump(fn, OUTPUT_STREAM::IO, args...)
xdump(io::IO, args...) = xdump(xdump, io, args...)
xdump(args...) = xdump(xdump, OUTPUT_STREAM::IO, args...)


# Here are methods specifically for dump:
dump(io::IO, x, n::Int) = dump(io, x, n, "")
dump(io::IO, x) = dump(io, x, 5, "")  # default is 5 levels
dump(args...) = dump(OUTPUT_STREAM::IO, args...)
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
dump(io::IO, x::AbstractKind, n::Int, indent) = println(io, x.name)
dump(io::IO, x::AbstractKind) = dumptype(io, x, 5, "")
dump(io::IO, x::AbstractKind, n::Int) = dumptype(io, x, n, "")
dump(io::IO, x::BitsKind, n::Int, indent) = println(io, x.name)
dump(io::IO, x::TypeVar, n::Int, indent) = println(io, x.name)


showall(x) = showall(OUTPUT_STREAM::IO, x)

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
    X::AbstractMatrix,
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

function print_matrix_row(io,
    X::AbstractMatrix, A::Vector,
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

function print_matrix_vdots(io,
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

function print_matrix(io,
    X::AbstractMatrix, rows::Integer, cols::Integer,
    pre::String, sep::String, post::String,
    hdots::String, vdots::String, ddots::String,
    hmod::Integer, vmod::Integer
)
    cols -= length(pre) + length(post)
    presp = repeat(" ", length(pre))
    postsp = ""
    @assert strwidth(hdots) == strwidth(ddots)
    ss = length(sep)
    m, n = size(X)
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
print_matrix(io, X::AbstractMatrix, rows::Integer, cols::Integer) =
    print_matrix(io, X, rows, cols, " ", "  ", "",
                 "  \u2026  ", "\u22ee", "  \u22f1  ", 5, 5)

print_matrix(io, X::AbstractMatrix) = print_matrix(io, X, tty_rows()-4, tty_cols())

summary(x) = string(typeof(x))

dims2string(d) = length(d) == 0 ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

summary{T}(a::AbstractArray{T}) =
    string(dims2string(size(a)), " ", T, " ", typeof(a).name)

function show_nd(io, a::AbstractArray)
    if isempty(a)
        return
    end
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(io, idxs...)
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
    cartesian_map((idxs...)->print_slice(io,idxs...), tail)
end

function whos(m::Module, pattern::Regex)
    for s in sort!(map(string, names(m)))
        v = symbol(s)
        if isdefined(m,v) && ismatch(pattern, s)
            println(rpad(s, 30), summary(eval(m,v)))
        end
    end
end
whos() = whos(r"")
whos(m::Module) = whos(m, r"")
whos(pat::Regex) = whos(ccall(:jl_get_current_module, Any, ())::Module, pat)

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
    cartesian_map(print_slice, tail)
end

function show_vector(io, v, opn, cls)
    X = reshape(v,(1,length(v)))
    print_matrix(io, X, 1, tty_cols(), opn, ", ", cls, "  \u2026  ", "\u22ee", "  \u22f1  ", 5, 5)
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

print_bit_chunk(c::Uint64, l::Integer) = print_bit_chunk(OUTPUT_STREAM, c, l)
print_bit_chunk(c::Uint64) = print_bit_chunk(OUTPUT_STREAM, c)

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
bitshow(B::BitArray) = bitshow(OUTPUT_STREAM, B)

bitstring(B::BitArray) = sprint(bitshow, B)
