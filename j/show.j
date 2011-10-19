# showing fundamental objects

print(x) = show(x)
show_to_string(x) = print_to_string(show, x)

show(s::Symbol) = print(s)
show(tn::TypeName) = show(tn.name)
show(::Nothing) = print("nothing")
show(b::Bool) = print(b ? "true" : "false")
show(n::Int)  = show(int64(n))
show(n::Uint) = show(uint64(n))

function show{T}(p::Ptr{T})
    if is(T,None)
        print("Ptr{Void}")
    else
        print(typeof(p))
    end
    if WORD_SIZE==64
        print(" @0x$(hex(uint(p),16))")
    else
        print(" @0x$(hex(uint(p), 8))")
    end
end

function show(l::LambdaStaticData)
    print("AST(")
    show(l.ast)
    print(")")
end

function show(tv::TypeVar)
    if !is(tv.lb, None)
        show(tv.lb)
        print("<:")
    end
    print(tv.name)
    if !is(tv.ub, Any)
        print("<:")
        show(tv.ub)
    end
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
                println(); newline=false
            else
                newline = true
            end
	end
    end
    print(close)
end

show_comma_array(itr, o, c) = show_delim_array(itr, o, ',', c, false)

function show(t::Tuple)
    show_delim_array(t, '(', ',', ')', true)
end

function show_expr_type(ty)
    if !is(ty, Any)
        if isa(ty, FuncKind)
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
        print("$(e.args[1])::$(e.args[2])")
    elseif is(hd,:body) || is(hd,:block)
        print("\nbegin\n")
        for a=e.args
            println("  $a")
        end
        println("end")
    elseif is(hd,:comparison)
        print(e.args...)
    elseif is(hd,:(.))
        print(e.args[1],'.',e.args[2])
    else
        print(hd)
        show_comma_array(e.args,'(',')')
    end
    show_expr_type(e.typ)
end

function show(e::SymbolNode)
    print(e.name)
    show_expr_type(e.typ)
end

show(e::LineNumberNode) = print("line($(e.line))")

show(e::LabelNode) = print("$(e.label): ")

show(e::GotoNode) = print("goto $(e.label)")

show(e::TopNode) = (print("top($(e.name))");
                    show_expr_type(e.typ))

function show(e::QuoteNode)
    a1 = e.value
    if isa(a1,Expr) && (is(a1.head,:body) || is(a1.head,:block))
        println("\nquote")
        for a=a1.args
            println("  $a")
        end
        println("end")
    else
        if isa(a1,Symbol) && !is(a1,:(:))
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
        print("type error: $(e.func): ",
              "$(ctx)expected $(e.expected), ",
              "got $(typeof(e.got))")
    end
end

function show(e::LoadError)
    show(e.error)
    print(" $(e.file):$(e.line)")
end

show(e::SystemError) = print("$(e.prefix): $(strerror(e.errnum))")
show(::DivideByZeroError) = print("error: integer divide by zero")
show(::StackOverflowError) = print("error: stack overflow")
show(::UndefRefError) = print("access to undefined reference")
show(::EOFError) = print("read: end of file")
show(e::ErrorException) = print(e.msg)
show(e::KeyError) = print("key not found: $(e.key)")
show(e::InterruptException) = nothing

function show(e::MethodError)
    name = ccall(:jl_genericfunc_name, Any, (Any,), e.f)
    if is(e.f,convert)
        print("no method $(name)(Type{$(e.args[1])},$(typeof(e.args[2])))")
    else
        print("no method $(name)$(typeof(e.args))")
    end
end

function show(e::UnionTooComplexError)
    print("union type pattern too complex: ")
    show(e.types)
end

function show(bt::BackTrace)
    show(bt.e)
    i = 1
    t = bt.trace
    while i < length(t)
        println()
        lno = t[i+2]
        if lno < 1
            line = ""
        else
            line = ":$lno"
        end
        print("in $(t[i]), $(t[i+1])$line")
        i += 3
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

function showall_matrix(a::AbstractArray)
    for i = 1:size(a,1)
        show_cols(a, 1, size(a,2), i)
        print('\n')
    end
end

function showall{T}(a::AbstractArray{T,2})
    print(summary(a))
    if isempty(a)
        return
    end
    println()
    showall_matrix(a)
end

function show{T}(a::AbstractArray{T,1})
    if is(T,Any)
        opn = '{'; cls = '}'
    else
        opn = '['; cls = ']';
    end
    n = size(a,1)
    if n <= 10
        show_comma_array(a, opn, cls)
    else
        show_comma_array(a[1:5], opn, "")
        print(",...,")
        show_comma_array(a[(n-4):n], "", cls)
    end
end

function show_cols(a, start, stop, i)
    for j = start:stop
        show(a[i,j])
        print(' ')
    end
end

function show{T}(a::AbstractArray{T,2})
    print(summary(a))
    if isempty(a)
        return
    end
    println()
    show_matrix(a)
end

alignment(x::Any) = (0, strlen(show_to_string(x)))
alignment(x::Number) = (strlen(show_to_string(x)), 0)
function alignment(x::Real)
    m = match(Regex(L"^(.*?)((?:[\.eE].*)?)$"), show_to_string(x))
    m == nothing ? (strlen(show_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Complex)
    m = match(Regex(L"^(.*?)(\+.*)$"), show_to_string(x))
    m == nothing ? (strlen(show_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(x::Rational)
    m = match(Regex(L"^(.*?/)(/.*)$"), show_to_string(x))
    m == nothing ? (strlen(show_to_string(x)), 0) :
                   (strlen(m.captures[1]), strlen(m.captures[2]))
end
function alignment(v::AbstractVector)
    l = r = 0
    for x = v
        a = alignment(x)
        l = max(l, a[1])
        r = max(r, a[2])
    end
    (l, r)
end

function alignment(
    X::AbstractMatrix,
    rows::AbstractVector, cols::AbstractVector,
    cols_if_complete::Int, cols_otherwise::Int, sep::Int
)
    a = {}
    for j = cols
        push(a, alignment(X[rows,j][:]))
        # TODO: remove [:] once X[rows,j] returns a vector
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
    i::Int, cols::AbstractVector, sep::String
)
    for k = 1:length(A)
        j = cols[k]
        x = X[i,j]
        a = alignment(x)
        l = repeat(" ", A[k][1]-a[1])
        r = repeat(" ", A[k][2]-a[2])
        print(l, show_to_string(x), r)
        if k < length(A); print(sep); end
    end
end

function print_matrix_vdots(vdots::String, A::Vector, sep::String, every::Int)
    for k = 1:length(A)
        if k % every == 1
            l = repeat(" ", A[k][1]-strlen(vdots))
            r = repeat(" ", A[k][2])
            print(l, vdots, r)
        else
            print(repeat(" ", A[k][1] + A[k][2]))
        end
        if k < length(A); print(sep); end
    end
end

function show_matrix_ng(
    X::AbstractMatrix, rows::Int, cols::Int,
    pre::String, sep::String, post::String,
    hdots::String, vdots::String
)
    println(summary(X))
    rows -= 1
    cols -= strlen(pre) + strlen(post)
    presp = repeat(" ", strlen(pre))
    postsp = ""
    ss = strlen(sep)
    m, n = size(X)
    if m <= rows # rows fit
        A = alignment(X,1:m,1:n,cols,cols,ss)
        if n <= length(A) # rows and cols fit
            for i = 1:m
                print(i == 1 ? pre : presp)
                print_matrix_row(X,A,i,1:n,sep)
                println(i == m ? post : postsp)
            end
        else # rows fit, cols don't
            c = div(cols-strlen(hdots)+1,2)+1
            R = reverse(alignment(X,1:m,n:-1:1,c,c,ss))
            c = cols - sum(map(sum,R)) - (length(R)-1)*ss - strlen(hdots)
            L = alignment(X,1:m,1:n,c,c,ss)
            for i = 1:m
                print(i == 1 ? pre : presp)
                print_matrix_row(X,L,i,1:length(L),sep)
                print(i % 5 == 1 ? hdots : repeat(" ", strlen(hdots)))
                print_matrix_row(X,R,i,n-length(R)+1:n,sep)
                println(i == m ? post : postsp)
            end
        end
    else # rows don't fit
        T = 1:div(rows,2)
        B = m-div(rows-1,2)+1:m
        A = alignment(X,[T,B],1:n,cols,cols,ss)
        if n <= length(A) # rows don't fit, cols do
            for i = [T,B]
                print(i == 1 ? pre : presp)
                print_matrix_row(X,A,i,1:n,sep)
                println(i == m ? post : postsp)
                if i == T[end]
                    print(i == 1 ? pre : presp)
                    print_matrix_vdots(vdots,A,sep,5)
                    println(i == m ? post : postsp)
                end
            end
        else # neither rows nor cols fit
            
        end
    end
end
show_matrix_ng(X::AbstractMatrix) =
    show_matrix_ng(X, tty_rows()-2, tty_cols()-1, " ", " ", "", "  ...  ", ":")

function show_matrix(a::AbstractArray)
    m = size(a,1)
    n = size(a,2)
    print_hdots = false
    print_vdots = false
    if 10 < m; print_vdots = true; end
    if 10 < n; print_hdots = true; end
    if !print_vdots && !print_hdots
        for i = 1:m
            show_cols(a, 1, n, i)
            if i < m; println(); end
        end
    elseif print_vdots && !print_hdots
        for i = 1:3
            show_cols(a, 1, n, i)
            println()
        end
        println(':')
        for i = m-2:m
            show_cols(a, 1, n, i)
            if i < m; println(); end
        end
    elseif !print_vdots && print_hdots
        for i = 1:m
            show_cols(a, 1, 3, i)
            if i == 1 || i == m; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            if i < m; println(); end
        end
    else
        for i = 1:3
            show_cols(a, 1, 3, i)
            if i == 1; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            println()
        end
        print(":\n")
        for i = m-2:m
            show_cols(a, 1, 3, i)
            if i == m; print(": "); else; print("  "); end
            show_cols(a, n-2, n, i)
            if i < m; println(); end
        end
    end
end

show{T}(a::AbstractArray{T,0}) = (println(summary(a)); show(a[]))

function show(a::AbstractArray)
    print(summary(a))
    if isempty(a)
        return
    end
    println()
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(idxs)
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
        show_matrix(a[:,:,idxs...])
        print(idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, map(x->Range1(1,x), tail))
end

function showall(a::AbstractArray)
    print(summary(a))
    if isempty(a)
        return
    end
    println()
    tail = size(a)[3:]
    nd = ndims(a)-2
    function print_slice(idxs)
        print("[:, :, ")
        for i = 1:(nd-1); print("$(idxs[i]), "); end
        println(idxs[end], "] =")
        showall_matrix(a[:,:,idxs...])
        print(idxs == tail ? "" : "\n\n")
    end
    cartesian_map(print_slice, map(x->Range1(1,x), tail))
end

summary(x) = string(typeof(x))

function dims2string(d)
    if length(d) == 0
        return "0-dimensional"
    elseif length(d) == 1
        strcat(d[1], "-element")
    else
        join("x", map(string,d))
    end
end

summary{T}(a::AbstractArray{T}) = strcat(dims2string(size(a)),
                                         " ", string(T), " ",
                                         string(typeof(a).name))

function whos()
    global VARIABLES
    for v = map(symbol,sort(map(string, VARIABLES)))
        if isbound(v)
            println(rpad(v, 30), summary(eval(v)))
        end
    end
end
