struct SparseMatrix{T} <: Matrix{T}
    m::Size
    n::Size
    numnz::Size
    colptr::Vector{Size}
    rowval::Vector{Size}
    nzval::Vector{T}
end

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

    return SparseMatrix(m,n,numnz,colptr,rowval,nzval)
end

nnz(S::SparseMatrix) = S.colptr[S.n+1]

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

function print(S::SparseMatrix)
    for col = 1:S.n
        for k = S.colptr[col]:S.colptr[col+1]-1
            print ("(", S.rowval[k], ",", col, ")   ", S.nzval[k], "\n")
        end
    end
end
