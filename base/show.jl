# formerly built-in methods. can be replaced any time.

show(x) = show(OUTPUT_STREAM::IOStream, x)

print(io::IOStream, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void}, Any,), io, s)
show(io, x) = isa(io,IOStream) ? ccall(:jl_show_any, Void, (Any,Any,), io, x) :
              print(io, repr(x))

showcompact(io, x) = show(io, x)
showcompact(x)     = showcompact(OUTPUT_STREAM::IOStream, x)

show(io, s::Symbol) = print(io, s)
show(io, tn::TypeName) = show(io, tn.name)
show(io, ::Nothing) = print(io, "nothing")
show(io, b::Bool) = print(io, b ? "true" : "false")
show(io, n::Integer) = (write(io, dec(n));nothing)
show(io, n::Unsigned) = print(io, "0x", hex(n,sizeof(n)<<1))

show{T}(io, p::Ptr{T}) =
    print(io, is(T,None) ? "Ptr{Void}" : typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

full_name(m::Module) = m===Root ? () : tuple(full_name(m.parent)...,m.name)

function show(io, m::Module)
    if is(m,Root)
        print(io, "Root")
    else
        print(io, join(full_name(m),"."))
    end
end

function show(io, l::LambdaStaticData)
    print(io, "AST(")
    show(io, l.ast)
    print(io, ")")
end

function show_delim_array(io, itr, op, delim, cl, delim_one)
    print(io, op)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
	while true
	    x, state = next(itr,state)
            multiline = isa(x,AbstractArray) && ndims(x)>1 && numel(x)>0
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

show_comma_array(io, itr, o, c) = show_delim_array(io, itr, o, ',', c, false)
show(io, t::Tuple) = show_delim_array(io, t, '(', ',', ')', true)

## Expr decoding helpers ##

is_expr(ex, head::Symbol) = (isa(ex, Expr) && (ex.head == head))
is_expr(ex, head::Symbol, n::Int) = is_expr(ex, head) && length(ex.args) == n

is_linenumber(ex::LineNumberNode) = true
is_linenumber(ex::Expr)           = is(ex.head, :line)
is_linenumber(ex)                 = false

is_quoted(ex::QuoteNode) = true
is_quoted(ex::Expr)      = is_expr(ex, :quote, 1)
is_quoted(ex)            = false

unquoted(ex::QuoteNode) = ex.value
unquoted(ex::Expr)      = ex.args[1]

## AST printing ##

function show_expr_type(io, ty)
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

show_linenumber(io::IO, line)       = print(io,"\t#  line ",line,':')
show_linenumber(io::IO, line, file) = print(io,"\t#  ",file,", line ",line,':')

show(io::IO, e::SymbolNode) = (print(io, e.name); show_expr_type(io, e.typ))
show(io::IO, e::LineNumberNode) = show_linenumber(io, e.line)
show(io::IO, e::LabelNode)      = print(io, e.label, ": ")
show(io::IO, e::GotoNode)       = print(io, "goto ", e.label)
show(io::IO, e::TopNode)        = print(io, "top(", e.name, ')')
show(io::IO, e::QuoteNode)      = show_quoted_expr(io, e.value)

# Show arguments of a block, and then body
function show_body(io::IO, args::Vector, body)
    print(io, indent(
            comma_list(args...),
            defer_io(show_body_lines, body)
        ))
end
show_body(io::IO, body)      = show_body(io, {},    body)
show_body(io::IO, arg, body) = show_body(io, {arg}, body)
defer_show_body(args...) = defer_io(show_body, args...)

# Show the body of a :block
function show_body_lines(io::IO, ex)
    args = is_expr(ex, :block) ? ex.args : {ex}
    for arg in args
        if !is_linenumber(arg); print(io, '\n'); end
        show(io, arg)
    end
end

const _expr_infix = Set(
    :(+=), :(-=), :(*=), :(/=), :(\=), :(&=), :(|=), :($=), 
    :(>>>=), :(>>=), :(<<=),
    :(=), :(:), :(<:), :(->), :(=>), :(&&), :(||), symbol("::"))
const _expr_calls  = {:ref =>('[',']'), :curly =>('{','}'), :call=>('(',')')}
const _expr_parens = {:vcat=>('[',']'), :cell1d=>('{','}')}

function show(io::IO, ex::Expr)
    head = ex.head
    args = ex.args
    nargs = length(args)

    if is(head, :(.))
        print(io, indent(args[1], '.'))
        if is_quoted(args[2]); show(io, unquoted(args[2]))
        else print(io, paren_block(defer_show(args[2])))
        end
    elseif has(_expr_infix, head) && nargs == 2       # infix operations
        print(io, indent(defer_show(args[1]), head, defer_show(args[2])))
    elseif is(head, :tuple)
        if nargs == 1; print(io, paren_block(defer_show(args[1]), ','))
        else           print(io, paren_block(comma_list(args...)))
        end
    elseif has(_expr_parens, head)                # :vcat/:cell1d
        print(io, _expr_parens[head][1], 
              indent(comma_list(args...)),
              _expr_parens[head][2])
    elseif has(_expr_calls, head) && nargs >= 1  # :call/:ref/:curly
        show(io, args[1]); 
        print(io, _expr_calls[head][1], 
              indent(comma_list(args[2:end]...)),
              _expr_calls[head][2])
    elseif is(head, :comparison) && nargs >= 2    # :comparison
        print(io, paren_block({defer_show(arg) for arg in args}...))
    elseif is(head, :(...)) && nargs == 1
        show(io, args[1]); print(io, "...")
    elseif (nargs == 1 && contains([:return, :abstract, :const], head)) ||
                          contains([:local, :global], head)
        print(io, head, ' ', indent(comma_list(args...)))
    elseif is(head, :typealias) && nargs == 2
        print(io, head, ' ', indent(
            defer_show(args[1]), ' ', defer_show(args[2])
        ))
    elseif is(head, :quote) && nargs == 1       # :quote
        show_quoted_expr(io, args[1])
    elseif is(head, :line) && (1 <= nargs <= 2) # :line
        show_linenumber(io, args...)
    elseif is(head, :if) && nargs == 3  # if/else
        print(io, 
            "if ",     defer_show_body(args[1], args[2]),
            "\nelse ", defer_show_body(args[3]),
            "\nend")
    elseif is(head, :try) && nargs == 3 # try[/catch]
        print(io, "try ", defer_show_body(args[1]))
        if !(is(args[2], false) && is_expr(args[3], :block, 0))
            print(io, "\ncatch ", defer_show_body(args[2], args[3]))
        end
        print(io, "\nend")
    elseif is(head, :let)               # :let 
        print(io, "let ", defer_show_body(args[2:end], args[1]), "\nend")
    elseif is(head, :block)
        print(io, "begin ", defer_show_body(ex), "\nend")
    elseif contains([:for, :while, :function, :if, :type], head) && nargs == 2
        print(io, head, ' ', defer_show_body(args[1], args[2]), "\nend")
    elseif is(head, :null)
        print(io, "nothing")
    elseif is(head, :gotoifnot)
        print(io, "unless ", defer_show(args[1]), " goto ",defer_show(args[2]))
    elseif is(head, :string)
        show(io, args[1])
    else
        print(io, head, paren_block(comma_list(args...)))
    end
    show_expr_type(io, ex.typ)
end

# show ex as if it were quoted
function show_quoted_expr(io::IO, sym::Symbol)
    if is(sym,:(:)) || is(sym,:(==)); print(io, ":($sym)")        
    else                              print(io, ":$sym")        
    end
end
function show_quoted_expr(io::IO, ex::Expr)
    if is(ex.head, :block); print(io, "quote ", defer_show_body(ex), "\nend")
    else                    print(io, "quote",  paren_block(defer_show(ex)))
    end
end
show_quoted_expr(io::IO, ex) = print(io, ':', paren_block(defer_show(ex)))


function show(io, e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected == Bool
        print(io, "type error: non-boolean ($(typeof(e.got))) ",
                  "used in boolean context")
    else
        if isa(e.got,Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        print(io, "type error: $(e.func): ",
                  "$(ctx)expected $(e.expected), ",
                  "got $tstr")
    end
end

show(io, e::LoadError) = (show(io, e.error); print(io, "\nat $(e.file):$(e.line)"))
show(io, e::SystemError) = print(io, "$(e.prefix): $(strerror(e.errnum))")
show(io, ::DivideByZeroError) = print(io, "error: integer divide by zero")
show(io, ::StackOverflowError) = print(io, "error: stack overflow")
show(io, ::UndefRefError) = print(io, "access to undefined reference")
show(io, ::EOFError) = print(io, "read: end of file")
show(io, e::ErrorException) = print(io, e.msg)
show(io, e::KeyError) = print(io, "key not found: $(e.key)")
show(io, e::InterruptException) = nothing

function show(io, e::MethodError)
    name = e.f.env.name
    if is(e.f,convert)
        print(io, "no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        print(io, "no method $(name)$(typeof(e.args))")
    end
end

function show(io, bt::BackTrace)
    show(io, bt.e)
    t = bt.trace
    # we may not declare :_jl_eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    const _jl_eval_function = symbol(string(_jl_eval_user_input))
    for i = 1:3:length(t)
        if i == 1 && t[i] == :error; continue; end
        if t[i] == _jl_eval_function; break; end
        print(io, "\n")
        lno = t[i+2]
        print(io, " in ", t[i], " at ", t[i+1])
        if lno >= 1
            print(io, ":", lno)
        end
    end
end

function show(io, m::Method)
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

function show(io, mt::MethodTable)
    name = mt.name
    println(io, "Methods for generic function ", name)
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

# dump & idump - structured tree representation like R's str()
# - dump is for the user-facing structure
# - idump is for the internal structure
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
# Package writers should not overload idump.

function idump(fn::Function, io::IOStream, x, n::Int, indent)
    T = typeof(x)
    print(io, T, " ")
    if isa(T, CompositeKind)
        println(io)
        if n > 0
            for field in T.names
                if field != symbol("")    # prevents segfault if symbol is blank
                    print(io, indent, "  ", field, ": ")
                    fn(io, getfield(x, field), n - 1, strcat(indent, "  "))
                end
            end
        end
    else
        println(io, x)
    end
end
function idump(fn::Function, io::IOStream, x::Array{Any}, n::Int, indent)
    println("Array($(eltype(x)),$(size(x)))")
    if n > 0
        for i in 1:(length(x) <= 10 ? length(x) : 5)
            print(io, indent, "  ", i, ": ")
            fn(io, x[i], n - 1, strcat(indent, "  "))
        end
        if length(x) > 10
            println(io, indent, "  ...")
            for i in length(x)-4:length(x)
                print(io, indent, "  ", i, ": ")
                fn(io, x[i], n - 1, strcat(indent, "  "))
            end
        end
    end
end
idump(fn::Function, io::IOStream, x::Symbol, n::Int, indent) = println(io, typeof(x), " ", x)
idump(fn::Function, io::IOStream, x::Function, n::Int, indent) = println(io, x)
idump(fn::Function, io::IOStream, x::Array, n::Int, indent) = println(io, "Array($(eltype(x)),$(size(x)))", " ", x)

# Types
idump(fn::Function, io::IOStream, x::UnionKind, n::Int, indent) = println(io, x)
function idump(fn::Function, io::IOStream, x::CompositeKind, n::Int, indent)
    println(io, x, "::", typeof(x), " ", " <: ", super(x))
    if n > 0
        for idx in 1:min(10,length(x.names))
            if x.names[idx] != symbol("")    # prevents segfault if symbol is blank
                print(io, indent, "  ", x.names[idx], "::")
                if isa(x.types[idx], CompositeKind) 
                    idump(fn, io, x.types[idx], n - 1, strcat(indent, "  "))
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

# _jl_dumptype is for displaying abstract type hierarchies like Jameson
# Nash's wiki page: https://github.com/JuliaLang/julia/wiki/Types-Hierarchy

function _jl_dumptype(io::IOStream, x::Type, n::Int, indent)
    # based on Jameson Nash's examples/typetree.jl
    println(io, x)
    if n == 0   # too deeply nested
        return  
    end
    typargs(t) = split(string(t), "{")[1]
    # todo: include current module?
    for m in (Core, Base)
        for s in names(m)
            if isbound(m,s)
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
                        _jl_dumptype(io, t, n - 1, strcat(indent, "  "))
                    end
                end
            end
        end
    end
end

# For abstract types, use _dumptype only if it's a form that will be called
# interactively.
idump(fn::Function, io::IOStream, x::AbstractKind) = _jl_dumptype(io, x, 5, "")
idump(fn::Function, io::IOStream, x::AbstractKind, n::Int) = _jl_dumptype(io, x, n, "")

# defaults:
idump(fn::Function, io::IOStream, x) = idump(idump, io, x, 5, "")  # default is 5 levels
idump(fn::Function, io::IOStream, x, n::Int) = idump(idump, io, x, n, "")
idump(fn::Function, args...) = idump(fn, OUTPUT_STREAM::IOStream, args...)
idump(io::IOStream, args...) = idump(idump, io, args...)
idump(args...) = idump(idump, OUTPUT_STREAM::IOStream, args...)


# Here are methods specifically for dump:
dump(io::IOStream, x, n::Int) = dump(io, x, n, "")
dump(io::IOStream, x) = dump(io, x, 5, "")  # default is 5 levels
dump(args...) = dump(OUTPUT_STREAM::IOStream, args...)
dump(io::IOStream, x::String, n::Int, indent) = println(io, typeof(x), " \"", x, "\"")
dump(io::IOStream, x, n::Int, indent) = idump(dump, io, x, n, indent)

function dump(io::IOStream, x::Dict, n::Int, indent)
    println(typeof(x), " len ", length(x))
    if n > 0
        i = 1
        for (k,v) in x
            print(io, indent, "  ", k, ": ")
            dump(io, v, n - 1, strcat(indent, "  "))
            if i > 10
                println(io, indent, "  ...")
                break
            end
            i += 1
        end
    end
end

# More generic representation for common types:
dump(io::IOStream, x::AbstractKind, n::Int, indent) = println(io, x.name)
dump(io::IOStream, x::AbstractKind) = _jl_dumptype(io, x, 5, "")
dump(io::IOStream, x::AbstractKind, n::Int) = _jl_dumptype(io, x, n, "")
dump(io::IOStream, x::BitsKind, n::Int, indent) = println(io, x.name)
dump(io::IOStream, x::TypeVar, n::Int, indent) = println(io, x.name)


showall(x) = showall(OUTPUT_STREAM::IOStream, x)

function showall{T}(io, a::AbstractArray{T,1})
    if is(T,Any)
        opn = '{'; cls = '}'
    else
        opn = '['; cls = ']';
    end
    show_comma_array(io, a, opn, cls)
end

alignment(x::Any) = (0, strlen(sprint(showcompact, x)))
alignment(x::Number) = (strlen(sprint(showcompact, x)), 0)
alignment(x::Integer) = (strlen(sprint(showcompact, x)), 0)
function alignment(x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Complex)
    m = match(r"^(.*,)(.*)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Rational)
    m = match(r"^(.*?/)(/.*)$", sprint(showcompact, x))
    m == nothing ? (strlen(sprint(showcompact, x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end

const _jl_undef_ref_str = "#undef"
const _jl_undef_ref_alignment = (3,3)

function alignment(
    X::AbstractMatrix,
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Integer, cols_otherwise::Integer, sep::Integer
)
    a = {}
    for j in cols
        l = r = 0
        for i in rows
            aij = _jl_undef_ref_alignment
            try
                aij = alignment(X[i,j])
            end
            l = max(l, aij[1])
            r = max(r, aij[2])
        end
        push(a, (l, r))
        if sum(map(sum,a)) + sep*length(a) >= cols_if_complete
            pop(a)
            break
        end
    end
    if length(a) < size(X,2)
        while sum(map(sum,a)) + sep*length(a) >= cols_otherwise
            pop(a)
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
        a = _jl_undef_ref_alignment
        sx = _jl_undef_ref_str
        try
            x = X[i,j]
            a = alignment(x)
            sx = sprint(showcompact, x)
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
            l = repeat(" ", max(0, A[k][1]-strlen(vdots)))
            r = repeat(" ", max(0, w-strlen(vdots)-strlen(l)))
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
    hdots::String, vdots::String,
    hmod::Integer, vmod::Integer
)
    cols -= strlen(pre) + strlen(post)
    presp = repeat(" ", strlen(pre))
    postsp = ""
    hdotssp = repeat(" ", strlen(hdots))
    ss = strlen(sep)
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
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
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
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,I,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,I,1:n,c,c,ss)
            r = (length(R)-n+1) % vmod
            for i in I
                print(io, i == 1 ? pre : presp)
                print_matrix_row(io, X,L,i,1:length(L),sep)
                print(io, i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
                print_matrix_row(io, X,R,i,n-length(R)+1:n,sep)
                print(io, i == m ? post : postsp)
                if i != I[end]; println(io, ); end
                if i == t
                    print(io, i == 1 ? pre : presp)
                    print_matrix_vdots(io, vdots,L,sep,vmod,1)
                    print(io, hdotssp)
                    print_matrix_vdots(io, vdots,R,sep,vmod,r)
                    println(io, i == m ? post : postsp)
                end
            end
        end
    end
end
print_matrix(io, X::AbstractMatrix, rows::Integer, cols::Integer) =
    print_matrix(io, X, rows, cols, " ", "  ", "", "  :  ", ":", 5, 5)

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
                if ii == 4 && allp(x->x==1,idxs[1:i-1])
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
    for s in sort(map(string, names(m)))
        v = symbol(s)
        if isbound(v) && ismatch(pattern, s)
            println(rpad(v, 30), summary(eval(m,v)))
        end
    end
end
whos() = whos(r"")
whos(m::Module) = whos(m, r"")
whos(pat::Regex) = whos(ccall(:jl_get_current_module, Module, ()), pat)

function show{T}(io, x::AbstractArray{T,0})
    println(io, summary(x),":")
    sx = _jl_undef_ref_str
    try
        sx = sprint(showcompact, x[])
    end
    print(io, sx)
end

function show(io, X::AbstractArray)
    print(io, summary(X))
    if !isempty(X)
        println(io, ":")
        ndims(X)==2 ? print_matrix(io, X) : show_nd(io, X)
    end
end

function showall(io, X::AbstractMatrix)
    print(io, summary(X))
    if !isempty(X)
        println(io, ":")
        print_matrix(io, X, typemax(Int64), typemax(Int64))
    end
end

function showall(io, a::AbstractArray)
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
    print_matrix(io, X, 1, tty_cols(), opn, ", ", cls, "  ...  ", ":", 5, 5)
end

show(io, v::AbstractVector{Any}) = show_vector(io, v, "{", "}")
show(io, v::AbstractVector)      = show_vector(io, v, "[", "]")


## Deferred IO for formatting etc ##

# Canned io action: print(io, defer_io(f, rest_args...))
# does the same as f(io, rest_args...)
defer_io(f, rest_args...) = DeferredIO(f, rest_args)

defer_print()        = ""
defer_print(arg)     = arg
defer_print(args...) = defer_io(print, args...)
defer_show(arg)      = defer_io(show,  arg)

indent(arg)          = Indented(arg)
indent(args...)      = Indented(defer_print(args...))

paren_block(args...) = defer_print('(', indent(args...), ')')

comma_list()         = ""
comma_list(args...)  = defer_io(show_comma_list, args...)


type DeferredIO
    f::Function
    rest_args::Tuple
end
print(io::IO, d::DeferredIO) = d.f(io, d.rest_args...)

type Indented
    item  # value to be printed indented
end

function show_comma_list(io::IO, first, rest...)
    show(io, first)
    for arg in rest; print(io, ", "); show(io, arg); end
end
show_comma_list(io::IO) = nothing


## IndentIO: indentation aware wrapper IO ##

const indent_width = 4

type IndentIO <: IO
    sink::IO
    indent::Integer  # current indentation
end
IndentIO(sink::IO) = IndentIO(sink, 0)

print(io::IO, ind::Indented) = print(IndentIO(io), ind)
function print(io::IndentIO, ind::Indented)
    io.indent += indent_width
    print(io, ind.item)
    io.indent -= indent_width
    nothing
end
function print(io::IndentIO, c::Char)
    print(io.sink, c)
    if (c == '\n'); print(io.sink, " "^io.indent); end
end

# Capture character output and send it to print(::IndentIO, ::Char)
write(io::IndentIO, x::Uint8)       = print(io, char(x))
write(io::IndentIO, s::ASCIIString) = (for c in s; print(io, c); end)
# Work around some types that do funky stuff in show()
show(io::IndentIO, x::Float32) = print(io, string(x))
show(io::IndentIO, x::Float64) = print(io, string(x))
show(io::IndentIO, x::Symbol)  = print(io, string(x))
