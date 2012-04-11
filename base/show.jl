# formerly built-in methods. can be replaced any time.
print(a::Array{Uint8,1}) = ccall(:jl_print_array_uint8, Void, (Any,), a)
print(s::Symbol) = ccall(:jl_print_symbol, Void, (Any,), s)
show(x) = ccall(:jl_show_any, Void, (Any,), x)

print(x) = show(x)
showcompact(x) = show(x)
show_to_string(x) = print_to_string(show, x)
showcompact_to_string(x) = print_to_string(showcompact, x)

show(s::Symbol) = print(s)
show(tn::TypeName) = show(tn.name)
show(::Nothing) = print("nothing")
show(b::Bool) = print(b ? "true" : "false")
show(n::Integer)  = print(dec(int64(n)))

function show_trailing_hex(n::Uint64, ndig::Integer)
    for s = ndig-1:-1:0
        d = (n >> 4*s) & uint64(0xf)
        print("0123456789abcdef"[d+1])
    end
end
show(n::Unsigned) = (print("0x"); show_trailing_hex(uint64(n),sizeof(n)<<1))

show{T}(p::Ptr{T}) =
    print(is(T,None) ? "Ptr{Void}" : typeof(p), " @0x$(hex(unsigned(p), WORD_SIZE>>2))")

function show(l::LambdaStaticData)
    print("AST(")
    show(l.ast)
    print(")")
end

function show_delim_array(itr, open, delim, close, delim_one)
    print(open)
    state = start(itr)
    newline = true
    first = true
    if !done(itr,state)
	while true
	    x, state = next(itr,state)
            multiline = isa(x,AbstractArray) && ndims(x)>1 && numel(x)>0
            if newline
                if multiline; println(); end
            end
	    show(x)
	    if done(itr,state)
                if delim_one && first
                    print(delim)
                end
		break
	    end
            first = false
            print(delim)
            if multiline
                println(); println(); newline=false
            else
                newline = true
            end
	end
    end
    print(close)
end

show_comma_array(itr, o, c) = show_delim_array(itr, o, ',', c, false)
show(t::Tuple) = show_delim_array(t, '(', ',', ')', true)

function show_expr_type(ty)
    if !is(ty, Any)
        if is(ty, Function)
            print("::F")
        elseif is(ty, IntrinsicFunction)
            print("::I")
        else
            print("::$ty")
        end
    end
end

function show(e::Expr)
    hd = e.head
    if is(hd,:call)
        print(e.args[1])
        show_comma_array(e.args[2:],'(',')')
    elseif is(hd,:(=))
        print("$(e.args[1]) = $(e.args[2])")
    elseif is(hd,:null)
        print("nothing")
    elseif is(hd,:gotoifnot)
        print("unless $(e.args[1]) goto $(e.args[2])")
    elseif is(hd,:return)
        print("return $(e.args[1])")
    elseif is(hd,:string)
        show(e.args[1])
    elseif is(hd,symbol("::"))
        show(e.args[1]); print("::"); show(e.args[2])
    elseif is(hd,:quote)
        show_quoted_expr(e.args[1])
    elseif is(hd,:body) || is(hd,:block)
        println("\nbegin")
        for a in e.args
            print("  "); show(a); println()
        end
        println("end")
    elseif is(hd,:comparison)
        for a in e.args; show(a); end
    elseif is(hd,:(.))
        show(e.args[1]); print('.'); show(e.args[2])
    else
        print(hd)
        show_comma_array(e.args,'(',')')
    end
    show_expr_type(e.typ)
end

show(e::SymbolNode) = (print(e.name); show_expr_type(e.typ))
show(e::LineNumberNode) = print("line($(e.line))")
show(e::LabelNode) = print("$(e.label): ")
show(e::GotoNode) = print("goto $(e.label)")
show(e::TopNode) = print("top($(e.name))")
show(e::QuoteNode) = show_quoted_expr(e.value)

function show_quoted_expr(a1)
    if isa(a1,Expr) && (is(a1.head,:body) || is(a1.head,:block))
        println("\nquote")
        for a in a1.args
            print("  "); show(a); println()
        end
        println("end")
    else
        if isa(a1,Symbol) && !is(a1,:(:)) && !is(a1,:(==))
            print(":$a1")
        else
            print(":($a1)")
        end
    end
end

function show(e::TypeError)
    ctx = isempty(e.context) ? "" : "in $(e.context), "
    if e.expected == Bool
        print("type error: non-boolean ($(typeof(e.got))) ",
              "used in boolean context")
    else
        if isa(e.got,Type)
            tstr = "Type{$(e.got)}"
        else
            tstr = string(typeof(e.got))
        end
        print("type error: $(e.func): ",
              "$(ctx)expected $(e.expected), ",
              "got $tstr")
    end
end

show(e::LoadError) = (show(e.error); print("\nat $(e.file):$(e.line)"))
show(e::SystemError) = print("$(e.prefix): $(strerror(e.errnum))")
show(::DivideByZeroError) = print("error: integer divide by zero")
show(::StackOverflowError) = print("error: stack overflow")
show(::UndefRefError) = print("access to undefined reference")
show(::EOFError) = print("read: end of file")
show(e::ErrorException) = print(e.msg)
show(e::KeyError) = print("key not found: $(e.key)")
show(e::InterruptException) = nothing

function show(e::MethodError)
    name = e.f.env.name
    if is(e.f,convert)
        print("no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        print("no method $(name)$(typeof(e.args))")
    end
end

function show(bt::BackTrace)
    show(bt.e)
    t = bt.trace
    # we may not declare :_jl_eval_user_input
    # directly so that we get a compile error
    # in case its name changes in the future
    const _jl_eval_function = symbol(string(_jl_eval_user_input))
    for i = 1:3:length(t)
        if i == 1 && t[i] == :error; continue; end
        if t[i] == _jl_eval_function; break; end
        print("\n")
        lno = t[i+2]
        print(" in ", t[i], " at ", t[i+1])
        if lno >= 1
            print(":", lno)
        end
    end
end

function dump(x)
    T = typeof(x)
    if isa(x,Array)
        print("Array($(eltype(x)),$(size(x)))")
    elseif isa(T,CompositeKind)
        print(T,'(')
        for field = T.names
            print(field, '=')
            dump(getfield(x, field))
            print(',')
        end
        println(')')
    else
        show(x)
    end
end

function showall{T}(a::AbstractArray{T,1})
    if is(T,Any)
        opn = '{'; cls = '}'
    else
        opn = '['; cls = ']';
    end
    show_comma_array(a, opn, cls)
end

alignment(x::Any) = (0, strlen(showcompact_to_string(x)))
alignment(x::Number) = (strlen(showcompact_to_string(x)), 0)
alignment(x::Integer) = (strlen(showcompact_to_string(x)), 0)
function alignment(x::Real)
    m = match(r"^(.*?)((?:[\.eE].*)?)$", showcompact_to_string(x))
    m == nothing ? (strlen(showcompact_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Complex)
    m = match(r"^(.*,)(.*)$", showcompact_to_string(x))
    m == nothing ? (strlen(showcompact_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Rational)
    m = match(r"^(.*?/)(/.*)$", showcompact_to_string(x))
    m == nothing ? (strlen(showcompact_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end

const _jl_undef_ref_str = "undef"
const _jl_undef_ref_alignment = (2,3)

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

function print_matrix_row(
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
            sx = showcompact_to_string(x)
        end
        l = repeat(" ", A[k][1]-a[1])
        r = repeat(" ", A[k][2]-a[2])
        print(l, sx, r)
        if k < length(A); print(sep); end
    end
end

function print_matrix_vdots(vdots::String, A::Vector, sep::String, M::Integer, m::Integer)
    for k = 1:length(A)
        w = A[k][1] + A[k][2]
        if k % M == m
            l = repeat(" ", max(0, A[k][1]-strlen(vdots)))
            r = repeat(" ", max(0, w-strlen(vdots)-strlen(l)))
            print(l, vdots, r)
        else
            print(repeat(" ", w))
        end
        if k < length(A); print(sep); end
    end
end

function print_matrix(
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
                print(i == 1 ? pre : presp)
                print_matrix_row(X,A,i,1:n,sep)
                print(i == m ? post : postsp)
                if i != m; println(); end
            end
        else # rows fit, cols don't
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                print(i == 1 ? pre : presp)
                print_matrix_row(X,L,i,1:length(L),sep)
                print(i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
                print_matrix_row(X,R,i,n-length(R)+1:n,sep)
                print(i == m ? post : postsp)
                if i != m; println(); end
            end
        end
    else # rows don't fit
        t = div(rows,2)
        I = [1:t; m-div(rows-1,2)+1:m]
        A = alignment(X,I,1:n,cols,cols,ss)
        if n <= length(A) # rows don't fit, cols do
            for i in I
                print(i == 1 ? pre : presp)
                print_matrix_row(X,A,i,1:n,sep)
                print(i == m ? post : postsp)
                if i != I[end]; println(); end
                if i == t
                    print(i == 1 ? pre : presp)
                    print_matrix_vdots(vdots,A,sep,vmod,1)
                    println(i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,I,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,I,1:n,c,c,ss)
            r = (length(R)-n+1) % vmod
            for i in I
                print(i == 1 ? pre : presp)
                print_matrix_row(X,L,i,1:length(L),sep)
                print(i % hmod == 1 ? hdots : repeat(" ", strlen(hdots)))
                print_matrix_row(X,R,i,n-length(R)+1:n,sep)
                print(i == m ? post : postsp)
                if i != I[end]; println(); end
                if i == t
                    print(i == 1 ? pre : presp)
                    print_matrix_vdots(vdots,L,sep,vmod,1)
                    print(hdotssp)
                    print_matrix_vdots(vdots,R,sep,vmod,r)
                    println(i == m ? post : postsp)
                end
            end
        end
    end
end
print_matrix(X::AbstractMatrix, rows::Integer, cols::Integer) =
    print_matrix(X, rows, cols, " ", "  ", "", "  :  ", ":", 5, 5)

print_matrix(X::AbstractMatrix) = print_matrix(X, tty_rows()-4, tty_cols())

summary(x) = string(typeof(x))

dims2string(d) = length(d) == 0 ? "0-dimensional" :
                 length(d) == 1 ? "$(d[1])-element" :
                 join(map(string,d), 'x')

summary{T}(a::AbstractArray{T}) =
    strcat(dims2string(size(a)), " ", string(T), " ", string(typeof(a).name))

function show_nd(a::AbstractArray)
    if isempty(a)
        return
    end
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(idxs...)
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
                    #println(idxs)
                    print("...\n\n")
                    return
                end
                if 3 < ii <= size(a,i+2)-3
                    return
                end
            end
        end
        print("[:, :, ")
        for i = 1:(nd-1); print("$(idxs[i]), "); end
        println(idxs[end], "] =")
        slice = a[:,:,idxs...]
        print_matrix(reshape(slice, size(slice,1), size(slice,2)))
        print(idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, tail)
end

function whos()
    global VARIABLES
    for v = map(symbol,sort(map(string, VARIABLES)))
        if isbound(v)
            println(rpad(v, 30), summary(eval(v)))
        end
    end
end

show{T}(x::AbstractArray{T,0}) = (println(summary(x),":"); show(x[]))
function show(X::AbstractArray)
    print(summary(X))
    if !isempty(X)
        println(":")
        ndims(X)==2 ? print_matrix(X) : show_nd(X)
    end
end

function showall(X::AbstractMatrix)
    print(summary(X))
    if !isempty(X)
        println(":")
        print_matrix(X, typemax(Int64), typemax(Int64))
    end
end

function showall(a::AbstractArray)
    print(summary(a))
    if isempty(a)
        return
    end
    println(":")
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(idxs...)
        print("[:, :, ")
        for i = 1:(nd-1); print("$(idxs[i]), "); end
        println(idxs[end], "] =")
        slice = a[:,:,idxs...]
        print_matrix(reshape(slice, size(slice,1), size(slice,2)),
                     typemax(Int64), typemax(Int64))
        print(idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, tail)
end

function show_vector(v, opn, cls)
    X = reshape(v,(1,length(v)))
    print_matrix(X, 1, tty_cols(), opn, ", ", cls, "  ...  ", ":", 5, 5)
end

show(v::AbstractVector{Any}) = show_vector(v, "{", "}")
show(v::AbstractVector)      = show_vector(v, "[", "]")
