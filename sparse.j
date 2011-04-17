type SparseArray2d{T} <: Matrix{T}
    m::Size
    n::Size
    colptr::Vector{Size}
    rowval::Vector{Size}
    nzval::Vector{T}
end

size(S::SparseArray2d) = (S.m, S.n)
nnz(S::SparseArray2d) = S.colptr[S.n+1] - 1

# TODO: Does not detect duplicate (i,j) values yet
# Assumes no such duplicates occur
function sparse{T}(I::Vector{Size}, 
                   J::Vector{Size}, 
                   V::Vector{T}, 
                   m::Size, 
                   n::Size)

    (I,p) = sortperm(I)
    J = J[p]
    V = V[p]

    (J,p) = sortperm(J)
    I = I[p]
    V = V[p]

    numnz = length(I)

    w = zeros(Size, n+1)    
    w[1] = 1
    for k=1:numnz; w[J[k] + 1] += 1; end
    colptr = cumsum(w)

    return SparseArray2d(m, n, colptr, I, V)
end

function find{T}(S::SparseArray2d{T})
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

sprand(m,n,density) = sprand_rng (m,n,density,rand)
sprandn(m,n,density) = sprand_rng (m,n,density,randn)
#sprandint(m,n,density) = sprand_rng (m,n,density,randint)

function sprand_rng(m, n, density, rng)
    numnz = int32(m*n*density)
    I = [ randint(1, m) | i=1:numnz ]
    J = [ randint(1, n) | i=1:numnz ]
    V = rng(numnz)
    S = sparse(I, J, V, m, n)
end

transpose(S::SparseArray2d) = ((I,J,V) = find(S); 
                               sparse (J, I, V, S.n, S.m) )

ctranspose(S::SparseArray2d) = ((I,J,V) = find(S); 
                                sparse (J, I, conj(V), S.n, S.m) )

function show(S::SparseArray2d)
    for col = 1 : S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            print('(')
            show(S.rowval[k])
            print(",$col)   ")
            show(S.nzval[k])
            println()
        end
    end
end

function convert{T}(::Type{Array{T}}, S::SparseArray2d{T})
    A = zeros(T, size(S))
    for col = 1 : S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            A[S.rowval[k], col] = S.nzval[k]
        end
    end
    return A

end

speye(n::Size) = sparse(linspace(1,n), linspace(1,n), ones(n), n, n)

function +{T1,T2}(A::SparseArray2d{T1}, B::SparseArray2d{T2})
    assert(size(A) == size(B))
    (m, n) = size(A)
    
    typeS = promote_type(T1, T2)
    # TODO: Need better method to allocate result
    nnzS = nnz(A) + nnz(B) 
    colptrS = Array(Size, A.n+1)
    rowvalS = Array(Size, nnzS)
    nzvalS = Array(typeS, nnzS)
    
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
                rowvalS[ptrS] = rowA
                nzvalS[ptrS] = nzvalA[ptrA]
                ptrA += 1
            elseif rowB < rowA
                rowvalS[ptrS] = rowB
                nzvalS[ptrS] = nzvalB[ptrB]
                ptrB += 1
            else
                rowvalS[ptrS] = rowA
                nzvalS[ptrS] = nzvalA[ptrA] + nzvalB[ptrB]
                ptrA += 1
                ptrB += 1
            end
            ptrS += 1
        end
        
        while ptrA < stopA
            rowA = rowvalA[ptrA]
            rowvalS[ptrS] = rowA
            nzvalS[ptrS] = nzvalA[ptrA]
            ptrA += 1
            ptrS += 1
        end
        
        while ptrB < stopB
            rowB = rowvalB[ptrB]
            rowvalS[ptrS] = rowB
            nzvalS[ptrS] = nzvalB[ptrB]
            ptrB += 1
            ptrS += 1
        end

        colptrS[col+1] = ptrS
    end
    
    return SparseArray2d(m, n, colptrS, rowvalS, nzvalS)
end
