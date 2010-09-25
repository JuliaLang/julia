struct SparseMatrix{T} <: Matrix{T}
    m::Size
    n::Size
    colptr::Vector{Size}
    rowval::Vector{Size}
    nzval::Vector{T}
end

nnz(S::SparseMatrix) = S.colptr[S.n+1] - 1

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

    rowval = copy(I)
    nzval = copy(V)

    return SparseMatrix(m,n,colptr,rowval,nzval)
end

function find{T}(S::SparseMatrix{T})
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

transpose(S::SparseMatrix) = ((I,J,V) = find(S); 
                              sparse (J, I, V, S.n, S.m) )
ctranspose(S::SparseMatrix) = ((I,J,V) = find(S); 
                               sparse (J, I, conj(V), S.n, S.m) )

function show(S::SparseMatrix)
    for col = 1 : S.n
        for k = S.colptr[col] : (S.colptr[col+1]-1)
            print("(")
            show(S.rowval[k])
            print(",", col, ")   ")
            show(S.nzval[k])
            print("\n")
        end
    end
end
