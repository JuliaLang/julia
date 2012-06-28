# formerly built-in methods. can be replaced any time.

show(x) = show(OUTPUT_STREAM::IOStream, x)

print(io::IOStream, s::Symbol) = ccall(:jl_print_symbol, Void, (Ptr{Void}, Any,), io, s)
show(io, x) = ccall(:jl_show_any, Void, (Any, Any,), io, x)

showcompact(io, x) = show(io, x)
showcompact(x)     = showcompact(OUTPUT_STREAM::IOStream, x)

show(io, s::Symbol) = print(io, s)
show(io, tn::TypeName) = show(io, tn.name)
show(io, ::Nothing) = print(io, "nothing")
show(io, b::Bool) = print(io, b ? "true" : "false")
show(io, n::Integer) = print(io, dec(n))
show(io, n::Unsigned) = print(io, "0x", hex(n,sizeof(n)<<1))

show{T}(io, p::Ptr{T}) =
    print(io, is(T,None) ? "Ptr{Void}" : typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

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

function show(io, e::Expr)
    hd = e.head
    if is(hd,:call)
        print(io, e.args[1])
        show_comma_array(io, e.args[2:],'(',')')
    elseif is(hd,:(=))
        print(io, "$(e.args[1]) = $(e.args[2])")
    elseif is(hd,:null)
        print(io, "nothing")
    elseif is(hd,:gotoifnot)
        print(io, "unless $(e.args[1]) goto $(e.args[2])")
    elseif is(hd,:return)
        print(io, "return $(e.args[1])")
    elseif is(hd,:string)
        show(io, e.args[1])
    elseif is(hd,symbol("::"))
        show(io, e.args[1])
        print(io, "::")
        show(io, e.args[2])
    elseif is(hd,:quote)
        show_quoted_expr(io, e.args[1])
    elseif is(hd,:body) || is(hd,:block)
        println(io, "\nbegin")
        for a in e.args
            print(io, "  ")
            show(io, a)
            println(io)
        end
        println(io, "end")
    elseif is(hd,:comparison)
        for a in e.args
            show(io, a)
        end
    elseif is(hd,:(.))
        show(io, e.args[1])
        print(io, '.')
        show(io, e.args[2])
    else
        print(io, hd)
        show_comma_array(io, e.args,'(',')')
    end
    show_expr_type(io, e.typ)
end

show(io, e::SymbolNode) = (print(io, e.name); show_expr_type(io, e.typ))
show(io, e::LineNumberNode) = print(io, "line($(e.line))")
show(io, e::LabelNode) = print(io, "$(e.label): ")
show(io, e::GotoNode) = print(io, "goto $(e.label)")
show(io, e::TopNode) = print(io, "top($(e.name))")
show(io, e::QuoteNode) = show_quoted_expr(io, e.value)

function show_quoted_expr(io, a1)
    if isa(a1,Expr) && (is(a1.head,:body) || is(a1.head,:block))
        println(io, "\nquote")
        for a in a1.args
            print(io, "  ")
            show(io, a)
            println(io, )
        end
        println(io, "end")
    else
        if isa(a1,Symbol) && !is(a1,:(:)) && !is(a1,:(==))
            print(io, ":$a1")
        else
            print(io, ":($a1)")
        end
    end
end

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
        for i in 1:min(10, length(x))
            print(io, indent, "  ", i, ": ")
            fn(io, x[i], n - 1, strcat(indent, "  "))
        end
    end
end
idump(fn::Function, io::IOStream, x::Symbol, n::Int, indent) = println(io, typeof(x), " ", x)
idump(fn::Function, io::IOStream, x::Function, n::Int, indent) = println(io, x)
idump(fn::Function, io::IOStream, x::Array, n::Int, indent) = println(io, "Array($(eltype(x)),$(size(x)))", " ", x[1:min(4,length(x))])

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
    # TODO: When namespaces are worked out, this probably needs fixing
    # to allow different modules to be included or to look in the
    # equivalent of R's search path.
    for s in [names(Core), names(Base)]  
        t = eval(s)
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
            if string(s) != string(t.name) # type aliases
                println(io, indent, "  ", s, " = ", t.name)
            elseif t != Any 
                print(io, indent, "  ")
                _jl_dumptype(io, t, n - 1, strcat(indent, "  "))
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

function whos(pattern::Regex)
    global VARIABLES
    for s = sort(map(string, VARIABLES))
        v = symbol(s)
        if isbound(v) && matches(pattern, s)
            println(rpad(v, 30), summary(eval(v)))
        end
    end
end
whos() = whos(r"")

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
