# This file is a part of Julia. License is MIT: http://julialang.org/license

# Test getindex for skinny sparse matrix
function sparse_getindex_skinny_perf()
    seed = 1
    srand(seed)

    # matsize = (size(A,1), nnz(A))
    matsize = [
        (2^12,1  ),
        (2^12,2^6),
        (2^18,1  ),
        (2^18,2^9)]
    # indsize = (length(I), number of repetitions)
    indsize = [
        (1,   2^12),
        (2^8, 2^8 ),
        (2^16,2^4 )]

    c = 0 # counter
    for (m,nz) in matsize
        A = sprand(m,1,nz/m)
        for (n,p) in indsize
            c += 1
            I = rand(1:m,n)
            @timeit indexing(A,I,p) "sparse_getindex_skinny$c" ""
        end
    end
end

function indexing(A,I,p)
    J = [1]
    for k = 1:p
        A[I,J]
    end
    nothing
end
