## Sparse matrix performance
include("../perfutil.jl")

## create some sparse matrices (need to be square):
seed = 1
srand(seed)

small = 10^3
med = 10^4
large = 10^5
huge = 10^6
# # 1 entry per line
# ss = []
# push!(ss, sprand(small, small, 1e-3))
# #push!(ss, sprand(med, med, 1e-4))
# push!(ss, sprand(large, large, 1e-5))

# 10 entries per line
ts = []
push!(ts, sprand(small, small, 1e-2))
#push!(ts, sprand(med, med, 1e-3))
push!(ts, sprand(large, large, 1e-4))

# 100 entries per line
us = []
push!(us, sprand(small, small, 1e-1))
#push!(us, sprand(med, med, 1e-2))
push!(us, sprand(large, large, 1e-3))
#push!(us, sprand(huge, huge, 1e-4))

# # 1000 entries per line
# vs = []
# push!(vs, sprand(small, small, 1.0))
# #push!(vs, sprand(med, med, 1e-1))
# push!(vs, sprand(large, large, 1e-2))
# #push!(vs, sprand(huge, huge, 1e-3))

## using uint32 (works up to 10^9)
uus = []
for u in us
    push!(uus, SparseMatrixCSC(u.m, u.n, uint32(u.colptr), uint32(u.rowval), u.nzval))
end

## getindex
rep = 20
reps = rep^2
function integer_indexing(A)
    # index with two random integers
    nI, nJ = size(A)
    rI = 1:nI
    rJ = 1:nJ
    tmp = zero(eltype(A))
    for i in rand(rI, reps)
        for j in rand(rJ, rep)
            tmp += A[i,j]
        end
    end
    tmp
end

function row_indexing(A, rowinds)
    # index rows with rowinds and columns with a random integer
    nI, nJ = size(A)
    rI = 1:nI
    rJ = 1:nJ
    tmp = zero(eltype(A))
    for j in rand(rJ, reps)
        tmp += sum(A[rowinds,j])
    end
    tmp
end

function col_indexing(A, colinds)
    # index rows with a random integer and columns with colinds
    nI, nJ = size(A)
    rI = 1:nI
    rJ = 1:nJ
    tmp = zero(eltype(A))
    for i in rand(rI, div(reps,10) )
        tmp += sum(A[i,colinds])
    end
    tmp
end

function row_col_indexing(A, rowinds, colinds)
    # index rows with rowinds and columns with colinds
    # we need:
    (maximum(rowinds)+rep < size(A,1) && maximum(colinds)+rep < size(A, 2)) || error("bad rowinds or colinds")
    nI, nJ = size(A)
    rI = 1:nI
    rJ = 1:nJ
    for i in 1:10
        for j in 1:10
            tmp2 = A[rowinds.+i, colinds.+j]
        end
    end
end

function one_arg_indexing(A, lininds)
    # This is for 1d-indexing and indexing with one array of logicals.
    # Both return a nx1 sparse matrix.
    tmp = zero(eltype(A))
    if isa(eltype(A), Bool)
        tmp = sum(A[lininds])
    else
        for i in 1:rep
            tmp += sum(A[lininds])
        end
    end
    tmp
end

# test performance with matrices in us, uus and ts for
# - integer indexing
# - range indexing
# - ordered array indexing
# - disordered array indexing
# - 1d indexing with integers and booleans

# setup:
# - indices
intinds = nothing
logicalinds = nothing # needs to be generated for a specific matrix size.
rangeinds = 121:237
orderedinds = [rangeinds]
disorderedinds = orderedinds[randperm(length(orderedinds))]

inds = [(intinds, "integers"), (logicalinds, "logical array"), (rangeinds, "a range"),
        (orderedinds, "a ordered array"), (disorderedinds, "a disordered array")]

# - matrix sizes
sizes = [(1, "small", "Small sparse matrix"), (2, "medium", "Medium sparse matrix")]

# - matrix types
mattyp = [(ts, "10 entries/column"), (us, "100 entries/column"), (uus, "100 entries/column uint32")]

# - functions
funs = [(integer_indexing, 1, "indexing"), (one_arg_indexing, 1, "1d indexing"),
        (row_indexing, 2, "indexing rows"), (col_indexing, 2, "indexing columns"),
        (row_col_indexing, 3, "indexing rows & columns")]

# performance tests:
counters = [1,1] # for small and medium matrix
# integer indexing
for (sz,s1,s2) in sizes # size of the matrix
    for (mt, ms) in mattyp  # type of the matrix
        m = mt[sz]
        c = counters[sz]
        @timeit integer_indexing(m) "sparse_getindex_$s1$c" "$s2 with $ms, indexing with integers"
        counters[sz] += 1
    end
end

# range & array indexing
for (sz,s1,s2) in sizes # size of the matrix
    for (mt, ms) in mattyp  # type of the matrix
        m = mt[sz]
        for (fun, nargs, funstr) in funs[3:5] # indexing test function
            for (ind,indstr) in inds[3:5] # type of indices
                c = counters[sz]
                if indstr=="logical array"
                    # make a logical array of the right size
                    ind = sprandbool(size(m,1)..., 1e-5)
                end
                if nargs==2
                    @timeit fun(m, ind)  "sparse_getindex_$s1$c" "Sparse matrix with $ms, $funstr with $indstr"
                elseif nargs==3
                    @timeit fun(m, ind, ind) "sparse_getindex_$s1$c" "Sparse matrix with $ms, $funstr with $indstr"
                else
                    error("Something is amiss here.")
                end
                counters[sz] += 1
            end
        end
    end
end

# linear indexing
for (sz,s1,s2) in sizes # size of the matrix
    for (mt, ms) in mattyp  # type of the matrix
        m = mt[sz]
        for (ind,indstr) in inds[2:5] # type of indices
            if indstr=="logical array"
                if sz==2
                    continue # logical indexing with medium size sparse matrix takes too long
                end
                # make a logical array of the right size
                ind = sprandbool(size(m)..., 1e-5)
                c = counters[sz]
                @timeit one_arg_indexing(m, ind) "sparse_getindex_$s1$c" "$s2 with $ms, linear indexing with $indstr"
                counters[sz] += 1
            else
                c = counters[sz]
                @timeit one_arg_indexing(m, ind) "sparse_getindex_$s1$c" "$s2 with $ms, linear indexing with $indstr"
                counters[sz] += 1
            end
        end
    end
end
