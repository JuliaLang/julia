# Compressed sparse columns data structure
# Assumes that no zeros are stored in the data structure
type SparseMatrixCSC{T} <: AbstractMatrix{T}
    m::Size                # Number of rows
    n::Size                # Number of columns
    colptr::Vector{Size}   # Column i is in colptr[i]:(colptr[i+1]-1)
    rowval::Vector{Size}   # Row values of nonzeros
    nzval::Vector{T}       # Nonzero values
end

issparse(A::AbstractArray) = false
issparse(S::SparseMatrixCSC) = true

size(S::SparseMatrixCSC) = (S.m, S.n)
nnz(S::SparseMatrixCSC) = S.colptr[end]-1

function show(S::SparseMatrixCSC)
    println(S.m, "-by-", S.n, " sparse matrix with ", nnz(S), " nonzeros:")
    for col = 1:S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            print("\t[")
            show(S.rowval[k])
            print(",\t", col, "]\t= ")
            show(S.nzval[k])
            println()
        end
    end
end

function convert{T}(::Type{Array{T}}, S::SparseMatrixCSC{T})
    A = zeros(T, size(S))
    for col = 1 : S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            A[S.rowval[k], col] = S.nzval[k]
        end
    end
    return A
end

full{T}(S::SparseMatrixCSC{T}) = convert(Array{T}, S)

function sparse(A::Matrix)
    m, n = size(A)
    I, J, V = findn_nzs(A)
    sparse(I, J, V, m, n)
end

sparse(I,J,V) = sparse(I, J, V, max(I), max(J))

function sparse(I::AbstractVector, J::AbstractVector, 
                V::Union(Number,AbstractVector), 
                m::Int, n::Int)

    create_J_copy = true

    if !issorted(I)
        (I,p) = sortperm(I)
        J = J[p]
        if isa(V, AbstractVector); V = V[p]; end
        create_J_copy = false
    else
        I = copy(I)
    end

    if !issorted(J)
        (J,p) = sortperm(J)
        I = I[p]
        if isa(V, AbstractVector); V = V[p]; end
    else
        if create_J_copy
            J = copy(J)
            if isa(V, AbstractVector); V = copy(V); end
        end
    end

    return _jl_make_sparse(I, J, V, m, n)

end

# _jl_make_sparse() assumes that I,J are sorted in dictionary order 
# (with J taking precedence)
# use sparse() with the same arguments if this is not the case
function _jl_make_sparse(I::AbstractVector, J::AbstractVector, 
                         V::Union(Number, AbstractVector),
                         m::Int, n::Int)

    if isa(I, Range1) || isa(I, Range); I = [I]; end
    if isa(J, Range1) || isa(J, Range); J = [J]; end

    if isa(V, Range1) || isa(V, Range)
        V = [V]
    elseif isa(V, Number)
        V = fill(V, length(I))
    end

    cols = zeros(Size, n+1)
    cols[1] = 1
    cols[J[1] + 1] = 1

    lastdup = 1
    ndups = 0
    I_lastdup = I[1]
    J_lastdup = J[1]

    for k=2:length(I)
        if I[k] == I_lastdup && J[k] == J_lastdup
            V[lastdup] += V[k]
            ndups += 1
        else
            cols[J[k] + 1] += 1
            lastdup = k-ndups
            if ndups != 0
                I_lastdup = I[k]
                J_lastdup = J[k]
                I[lastdup] = I_lastdup
                V[lastdup] = V[k]
            end
        end
    end

    colptr = cumsum(cols)

    if ndups > 0.2*length(I)
        numnz = length(I)-ndups
        I = I[1:numnz]
        V = V[1:numnz]
    end

    return SparseMatrixCSC(m, n, colptr, I, V)
end

function find{T}(S::SparseMatrixCSC{T})
    numnz = nnz(S)
    I = Array(Size, numnz)
    J = Array(Size, numnz)
    V = Array(T, numnz)

    count = 1
    for col = 1 : S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            if S.nzval[k] != 0 
                I[count] = S.rowval[k]
                J[count] = col
                V[count] = S.nzval[k]
                count += 1
            else
                println("Warning: sparse matrix has explicit stored zeros.")
            end
        end
    end

    if numnz != count-1
        I = I[1:count]
        J = J[1:count]
        V = V[1:count]
    end

    return (I, J, V)
end

function sprand_rng(m, n, density, rng)
    numnz = long(m*n*density)
    I = [ randi_interval(1, m) | i=1:numnz ]
    J = [ randi_interval(1, n) | i=1:numnz ]
    V = rng((numnz,))
    S = sparse(I, J, V, m, n)
end

sprand(m,n,density) = sprand_rng (m,n,density,rand)
sprandn(m,n,density) = sprand_rng (m,n,density,randn)
#sprandi(m,n,density) = sprand_rng (m,n,density,randi)

speye(n::Size) = ( L = linspace(1,n); _jl_make_sparse(L, L, ones(Float64, n), n, n) )
speye(m::Size, n::Size) = ( x = min(m,n); L = linspace(1,x); _jl_make_sparse(L, L, ones(Float64, x), m, n) )

function issymmetric(A::SparseMatrixCSC)
    # Slow implementation
    nnz(A - A.') == 0 ? true : false
end

transpose(S::SparseMatrixCSC) = ( (I,J,V) = find(S); sparse(J, I, V, S.n, S.m) )
ctranspose(S::SparseMatrixCSC) = ( (I,J,V) = find(S); sparse(J, I, conj(V), S.n, S.m) )

macro _jl_binary_op_A_sparse_B_sparse_res_sparse(op)
    quote

        function ($op){T1,T2}(A::SparseMatrixCSC{T1}, B::SparseMatrixCSC{T2})
            assert(size(A) == size(B))
            (m, n) = size(A)

            typeS = promote_type(T1, T2)
            # TODO: Need better method to allocate result
            nnzS = nnz(A) + nnz(B)
            colptrS = Array(Size, A.n+1)
            rowvalS = Array(Size, nnzS)
            nzvalS = Array(typeS, nnzS)

            zero = convert(typeS, 0)

            colptrA = A.colptr
            rowvalA = A.rowval
            nzvalA = A.nzval

            colptrB = B.colptr
            rowvalB = B.rowval
            nzvalB = B.nzval

            ptrS = 1
            colptrS[1] = 1

            for col = 1:n
                ptrA = colptrA[col]
                stopA = colptrA[col+1]
                ptrB = colptrB[col]
                stopB = colptrB[col+1]

                while ptrA < stopA && ptrB < stopB
                    rowA = rowvalA[ptrA]
                    rowB = rowvalB[ptrB]
                    if rowA < rowB
                        res = ($op)(nzvalA[ptrA], zero)
                        if res != zero
                            rowvalS[ptrS] = rowA
                            nzvalS[ptrS] = ($op)(nzvalA[ptrA], zero)
                            ptrS += 1
                        end
                        ptrA += 1
                    elseif rowB < rowA
                        res = ($op)(zero, nzvalB[ptrB])
                        if res != zero
                            rowvalS[ptrS] = rowB
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrB += 1
                    else
                        res = ($op)(nzvalA[ptrA], nzvalB[ptrB])
                        if res != zero
                            rowvalS[ptrS] = rowA
                            nzvalS[ptrS] = res
                            ptrS += 1
                        end
                        ptrA += 1
                        ptrB += 1
                    end
                end

                while ptrA < stopA
                    res = ($op)(nzvalA[ptrA], zero)
                    if res != zero
                        rowA = rowvalA[ptrA]
                        rowvalS[ptrS] = rowA
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrA += 1
                end

                while ptrB < stopB
                    res = ($op)(zero, nzvalB[ptrB])
                    if res != zero
                        rowB = rowvalB[ptrB]
                        rowvalS[ptrS] = rowB
                        nzvalS[ptrS] = res
                        ptrS += 1
                    end
                    ptrB += 1
                end

                colptrS[col+1] = ptrS
            end

            # Free up unused memory before returning?
            return SparseMatrixCSC(m, n, colptrS, rowvalS, nzvalS)
        end

    end # quote
end # macro

(+)(A::SparseMatrixCSC, B::Union(Array,Number)) = (+)(full(A), B)
(+)(A::Union(Array,Number), B::SparseMatrixCSC) = (+)(A, full(B))
@_jl_binary_op_A_sparse_B_sparse_res_sparse (+)

(-)(A::SparseMatrixCSC, B::Union(Array,Number)) = (-)(full(A), B)
(-)(A::Union(Array,Number), B::SparseMatrixCSC) = (-)(A, full(B))
@_jl_binary_op_A_sparse_B_sparse_res_sparse (-)

(.*)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, A.colptr, A.rowval, A.nzval .* B)
(.*)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, B.colptr, B.rowval, A .* B.nzval)
(.*)(A::SparseMatrixCSC, B::Array) = (.*)(A, sparse(B))
(.*)(A::Array, B::SparseMatrixCSC) = (.*)(sparse(A), B)
@_jl_binary_op_A_sparse_B_sparse_res_sparse (.*)

(./)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, A.colptr, A.rowval, A.nzval ./ B)
(./)(A::Number, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::Array) = (./)(full(A), B)
(./)(A::Array, B::SparseMatrixCSC) = (./)(A, full(B))
(./)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (./)(full(A), full(B))

(.\)(A::SparseMatrixCSC, B::Number) = (.\)(full(A), B)
(.\)(A::Number, B::SparseMatrixCSC) = SparseMatrixCSC(B.m, B.n, B.colptr, B.rowval, B.nzval .\ A)
(.\)(A::SparseMatrixCSC, B::Array) = (.\)(full(A), B)
(.\)(A::Array, B::SparseMatrixCSC) = (.\)(A, full(B))
(.\)(A::SparseMatrixCSC, B::SparseMatrixCSC) = (.\)(full(A), full(B))

(.^)(A::SparseMatrixCSC, B::Number) = SparseMatrixCSC(A.m, A.n, A.colptr, A.rowval, A.nzval .^ B)
(.^)(A::Number, B::SparseMatrixCSC) = (.^)(A, full(B))
(.^)(A::SparseMatrixCSC, B::Array) = (.^)(full(A), B)
(.^)(A::Array, B::SparseMatrixCSC) = (.^)(A, full(B))
@_jl_binary_op_A_sparse_B_sparse_res_sparse (.^)

## ref ##
ref(A::SparseMatrixCSC, i::Int) = ref(A, ind2sub(size(A),i))
ref(A::SparseMatrixCSC, I::(Int,Int)) = ref(A, I[1], I[2])

function ref{T}(A::SparseMatrixCSC{T}, i0::Int, i1::Int)
    if i0 < 1 || i0 > A.m || i1 < 1 || i1 > A.n; error("ref: index out of bounds"); end
    first = A.colptr[i1]
    last = A.colptr[i1+1]-1
    while first <= last
        mid = (first + last) >> 1
        t = A.rowval[mid]
        if t == i0
            return A.nzval[mid]
        elseif t > i0
            last = mid - 1
        else
            first = mid + 1
        end
    end
    return zero(T)
end

ref{T<:Int}(A::SparseMatrixCSC, I::AbstractVector{T}, J::AbstractVector{T}) = _jl_sparse_ref(A,I,J)
ref(A::SparseMatrixCSC, I::AbstractVector, J::AbstractVector) = _jl_sparse_ref(A,I,J)

function _jl_sparse_ref(A::SparseMatrixCSC, I::AbstractVector, J::AbstractVector)

    (nr, nc) = size(A)
    nI = length(I)
    nJ = length(J)

    is_I_colon = (isa(I, Range1) || isa(I, Range)) && I.start == 1 && I.stop == nr && step(I) == 1
    is_J_colon = (isa(J, Range1) || isa(J, Range)) && J.start == 1 && J.stop == nc && step(J) == 1

    if is_I_colon && is_J_colon
        return A
    elseif is_J_colon 
        IM = sparse (1:nI, I, 1, nI, nr)
        B = IM * A
    elseif is_I_colon
        JM = sparse (J, 1:nJ, 1, nc, nJ)
        B = A *JM
    else
        IM = sparse (1:nI, I, 1, nI, nr)
        JM = sparse (J, 1:nJ, 1, nc, nJ)
        B = IM * A * JM
    end

end

#assign
assign{T,N}(A::SparseMatrixCSC{T},v::AbstractArray{T,N},i::Int) =
    invoke(assign, (SparseMatrixCSC{T}, Any, Int), A, v, i)
assign{T,N}(A::SparseMatrixCSC, v::AbstractArray{T,N}, i0::Int, i1::Int) = 
    invoke(assign, (SparseMatrixCSC{T}, Any, Int, Int), A, v, i0, i1)
assign{T}(A::SparseMatrixCSC{T}, v, i::Int) = assign(A, v, ind2sub(size(A),i))
assign{T}(A::SparseMatrixCSC{T}, v, I::(Int,Int)) = assign(A, v, I[1], I[2])

function assign{T}(A::SparseMatrixCSC{T}, v, i0::Int, i1::Int)
    if i0 < 1 || i0 > A.m || i1 < 1 || i1 > A.n; error("assign: index out of bounds"); end
    if v == zero(T) #either do nothing or delete entry if it exists
        first = A.colptr[i1]
        last = A.colptr[i1+1]-1
        loc = -1
        while first <= last
            mid = (first + last) >> 1
            t = A.rowval[mid]
            if t == i0
                loc = mid
                break
            elseif t > i0
                last = mid - 1
            else
                first = mid + 1
            end
        end
        if loc != -1
            del(A.rowval, loc)
            del(A.nzval, loc)
            for j = (i1+1):(A.n+1)
                A.colptr[j] = A.colptr[j] - 1
            end
        end
        return A
    end
    first = A.colptr[i1]
    last = A.colptr[i1+1]-1
    #find i such that A.rowval[i] = i0, or A.rowval[i-1] < i0 < A.rowval[i]
    while last - first >= 3
        mid = (first + last) >> 1
        t = A.rowval[mid]
        if t == i0
            A.nzval[mid] = v
            return A
        elseif t > i0
            last = mid - 1
        else
            first = mid + 1
        end
    end
    if last - first == 2
        mid = first + 1
        if A.rowval[mid] == i0
            A.nzval[mid] = v
            return A
        elseif A.rowval[mid] > i0
            if A.rowval[first] == i0
                A.nzval[first] = v
                return A
            elseif A.rowval[first] < i0
                i = first+1
            else #A.rowval[first] > i0
                i = first
            end
        else #A.rowval[mid] < i0
            if A.rowval[last] == i0
                A.nzval[last] = v
                return A
            elseif A.rowval[last] > i0
                i = last
            else #A.rowval[last] < i0
                i = last+1
            end
        end
    elseif last - first == 1
        if A.rowval[first] == i0
            A.nzval[first] = v
            return A
        elseif A.rowval[first] > i0
            i = first
        else #A.rowval[first] < i0
            if A.rowval[last] == i0
                A.nzval[last] = v
                return A
            elseif A.rowval[last] < i0
                i = last+1
            else #A.rowval[last] > i0
                i = last
            end
        end
    elseif last == first
        if A.rowval[first] == i0
            A.nzval[first] = v
            return A
        elseif A.rowval[first] < i0
            i = first+1
        else #A.rowval[first] > i0
            i = first
        end
    else #last < first to begin with
        i = first
    end
    insert(A.rowval, i, i0)
    insert(A.nzval, i, v)
    for j = (i1+1):(A.n+1)
        A.colptr[j] = A.colptr[j] + 1
    end
    return A
end

#TODO: assign of ranges, vectors
#TODO: sub

# In matrix-vector multiplication, the right orientation of the vector is assumed.
function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Vector{T2})
    Y = zeros(promote_type(T1,T2), A.m)
    for col = 1 : A.n
        for k = A.colptr[col] : (A.colptr[col+1]-1)
            Y[A.rowval[k]] += A.nzval[k] * X[col]
        end
    end
    return Y
end

# In vector-matrix multiplication, the right orientation of the vector is assumed.
function (*){T1,T2}(X::Vector{T1}, A::SparseMatrixCSC{T2})
    Y = zeros(promote_type(T1,T2), A.n)
    for col = 1 : A.n
        for k = A.colptr[col] : (A.colptr[col+1]-1)
            Y[col] += X[A.rowval[k]] * A.nzval[k]
        end
    end
    return Y
end

function (*){T1,T2}(A::SparseMatrixCSC{T1}, X::Matrix{T2})
    mX, nX = size(X)
    if A.n != mX; error("error in *: mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), A.m, nX)
    for multivec_col = 1:nX
        for col = 1 : A.n
            for k = A.colptr[col] : (A.colptr[col+1]-1)
                Y[A.rowval[k], multivec_col] += A.nzval[k] * X[col, multivec_col]
            end
        end
    end
    return Y
end

function (*){T1,T2}(X::Matrix{T1}, A::SparseMatrixCSC{T2})
    mX, nX = size(X)
    if nX != A.m; error("error in *: mismatched dimensions"); end
    Y = zeros(promote_type(T1,T2), mX, A.n)
    for multivec_row = 1:mX
        for col = 1 : A.n
            for k = A.colptr[col] : (A.colptr[col+1]-1)
                Y[multivec_row, col] += X[multivec_row, A.rowval[k]] * A.nzval[k]
            end
        end
    end
    return Y
end

# sparse * sparse
function (*){T1,T2}(X::SparseMatrixCSC{T1},Y::SparseMatrixCSC{T2}) 
    mX, nX = size(X)
    mY, nY = size(Y)
    if nX != mY; error("error in *: mismatched dimensions"); end
    T = promote_type(T1,T2)

    colptr = Array(Size, nY+1)
    colptr[1] = 1
    rowval = Array(Size, nnz(X) + nnz(Y))  # Need better estimation of space for result
    nzval = Array(T, nnz(X) + nnz(Y))      # Need better estimation of space for result

    spa = SparseAccumulator(T, mX);
    for y_col = 1:nY
        for y_elt = Y.colptr[y_col] : (Y.colptr[y_col+1]-1)
            x_col = Y.rowval[y_elt]
            _jl_spa_axpy(spa, Y.nzval[y_elt], X, x_col)
        end
        (rowval, nzval) = _jl_spa_store_reset(spa, y_col, colptr, rowval, nzval)
    end
        
    return SparseMatrixCSC(mX, nY, colptr, rowval, nzval)     
end

## SparseAccumulator and related functions

type SparseAccumulator{T} <: AbstractVector{T}
    vals::Vector{T}
    flags::Vector{Bool}
    indexes::Vector{Index}
    nvals::Size
end

show{T}(S::SparseAccumulator{T}) = invoke(show, (Any,), S)

SparseAccumulator{T}(::Type{T}, s::Size) = SparseAccumulator(zeros(T,s), falses(s), Array(Index,s), 0)
SparseAccumulator(s::Size) = SparseAccumulator(Float64, s)

size(S::SparseAccumulator) = length(S.vals)
length(S::SparseAccumulator) = length(S.vals)
numel(S::SparseAccumulator) = S.nvals

# reset spa
function _jl_spa_reset{T}(S::SparseAccumulator{T})
    z = zero(T)
    for i=1:numel(S)
        S.vals[i] = z
        S.flags[i] = false
    end
    S.nvals = 0
end

# store spa and reset
function _jl_spa_store_reset{T}(S::SparseAccumulator{T}, col, colptr, rowval, nzval)
    z = zero(T)
    start = colptr[col]

    if numel(S) > length(nzval) - start 
        rowval = grow(rowval, 2*length(rowval))
        nzval = grow(nzval, 2*length(nzval))
    end

    for i=1:numel(S)
        pos = S.indexes[i]
        rowval[start + i - 1] = pos
        nzval[start + i - 1] = S.vals[pos]
        S.vals[pos] = z
        S.flags[i] = false
    end

    colptr[col+1] = start + numel(S)
    S.nvals = 0
    return (rowval, nzval)
end

ref{T}(S::SparseAccumulator{T}, i::Index) = S.flags[i] ? S.vals[i] : zero(T)

assign{T,N}(S::SparseAccumulator{T}, v::AbstractArray{T,N}, i::Index) = 
    invoke(assign, (SparseAccumulator{T}, Any, Index), S, v, i)

function assign{T}(S::SparseAccumulator{T}, v, i::Index)
    if v == zero(T)
        if S.flags[i]
            S.vals[i] = v
            S.flags[i] = false
            S.nvals -= 1
        end
    else
        if S.flags[i]
            S.vals[i] = v
        else
            S.flags[i] = true
            S.vals[i] = v
            S.nvals += 1
            S.indexes[S.nvals] = i
        end
    end
    return S
end

#increments S[i] by v
function _jl_spa_incr{T}(S::SparseAccumulator{T}, v, i::Index)
    if v != zero(T)
        if S.flags[i]
            S.vals[i] += v
        else
            S.flags[i] = true
            S.vals[i] = v
            S.nvals += 1
            S.indexes[S.nvals] = i            
        end
    end
    return S
end

#sets S = S + a*x, where x is col j of A
function _jl_spa_axpy(S::SparseAccumulator, a, A::SparseMatrixCSC, j::Index)
    if size(S) != A.m; error("error in _jl_spa_axpy: dimension mismatch"); end
    for i = A.colptr[j]:(A.colptr[j+1]-1)
        _jl_spa_incr(S, a*A.nzval[i], A.rowval[i])
    end
    return S
end
