type SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    m::Int                  # Number of rows
    rowval::Vector{Ti}      # Indices of nonzeros
    nzval::Vector{Tv}       # Nonzero values
end

Base.size(S::SparseVector) = (S.m,)

nnz(S::SparseVector) = int(length(S.nzval))
Base.countnz(S::SparseVector) = countnz(S.nzval)

nonzeros(S::SparseVector) = S.nzval

function Base.showarray(io::IO, S::SparseVector;
                        header::Bool=true, limit::Bool=Base._limit_output,
                        rows::Int = Base.tty_size()[1], repr::Bool=false)
    # TODO: repr?

    if header
        print(io, S.m, "-element sparse vector with ", nnz(S), " ", eltype(S), " entries:")
    end

    if limit
        half_screen_rows = div(rows - 8, 2)
    else
        half_screen_rows = typemax(Int)
    end
    pad = ndigits(S.m)
    k = 0
    sep = "\n\t"
    for k = 1:length(S.rowval)
        if k < half_screen_rows || k > nnz(S)-half_screen_rows
            print(io, sep, '[', lpad(S.rowval[k], pad), "]  =  ")
            showcompact(io, S.nzval[k])
        elseif k == half_screen_rows
            print(io, sep, '\u22ee')
        end
    end
end

Base.show(io::IO, S::SparseVector) = Base.showarray(io,S)
Base.writemime(io::IO, ::MIME"text/plain", S::SparseVector) = show(io,S)

# Reinterpret
function Base.reinterpret{T,Tv,Ti}(::Type{T}, a::SparseVector{Tv,Ti})
    if sizeof(T) != sizeof(Tv)
        throw(ArgumentError("SparseVector reinterpret is only supported for element types of the same size"))
    end
    rowval = copy(a.rowval)
    nzval  = reinterpret(T, a.nzval)
    return SparseVector{T,Ti}(a.m, rowval, nzval)
end

## Constructors
Base.copy(S::SparseVector) = SparseVector(S.m, copy(S.rowval), copy(S.nzval))

Base.similar(S::SparseVector, Tv::Base.NonTupleType=eltype(S))   = SparseVector(S.m, copy(S.rowval), Array(Tv, length(S.nzval)))
Base.similar{Tv,Ti,TvNew}(S::SparseVector{Tv,Ti}, ::Type{TvNew}, ::Type{Ti}) = similar(S, TvNew)
Base.similar{Tv,Ti,TvNew,TiNew}(S::SparseVector{Tv,Ti}, ::Type{TvNew}, ::Type{TiNew}) = SparseVector(S.m, convert(Array{TiNew}, S.rowval), Array(TvNew, length(S.nzval)))
Base.similar{Tv}(S::SparseVector, ::Type{Tv}, d::NTuple{Integer}) = spzeros(Tv, d...)

function Base.convert{Tv,Ti,TvS,TiS}(::Type{SparseVector{Tv,Ti}}, S::SparseVector{TvS,TiS})
    if Tv == TvS && Ti == TiS
        return S
    else
        return SparseVector(S.m,
                               convert(Vector{Ti},S.rowval),
                               convert(Vector{Tv},S.nzval))
    end
end

function Base.convert{Tv,TvS,TiS}(::Type{SparseVector{Tv}}, S::SparseVector{TvS,TiS})
    if Tv == TvS
        return S
    else
        return SparseVector(S.m, S.rowval,
                            convert(Vector{Tv},S.nzval))
    end
end

function Base.findnz{T}(A::Vector{T})
    nnzA = countnz(A)
    I = zeros(Int, nnzA)
    NZs = zeros(T, nnzA)
    count = 1
    if nnzA > 0
        @inbounds for i=1:length(A)
            Ai = A[i]
            if Ai != 0
                I[count] = i
                NZs[count] = Ai
                count += 1
            end
        end
    end
    return I, NZs
end

function sparse_I_sorted!{Ti<:Integer}(I::AbstractVector{Ti}, V,
                                        m::Integer, combine::Function=(+))

    m = m < 0 ? 0 : m
    length(V) == 0 && return spzeros(eltype(V),Ti,m)

    lastdup = 1
    ndups = 0
    I_lastdup = I[1]
    L = length(I)

    @inbounds for k=2:L
        if I[k] == I_lastdup
            V[lastdup] = combine(V[lastdup], V[k])
            ndups += 1
        else
            lastdup = k-ndups
            I_lastdup = I[k]
            if ndups != 0
                I[lastdup] = I_lastdup
                V[lastdup] = V[k]
            end
        end
    end

    # Allow up to 20% slack
    if ndups > 0.2*L
        numnz = L-ndups
        deleteat!(I, (numnz+1):L)
        deleteat!(V, (numnz+1):length(V))
    end

    return SparseVector(m, I, V)
end

function Base.convert{Tv,Ti}(::Type{SparseVector{Tv,Ti}}, V::Vector)
    m = length(V)
    I, V = findnz(V)
    return sparse_I_sorted!(convert(Vector{Ti},I),
                            convert(Vector{Tv},V),
                             m)
end
Base.convert{T}(::Type{AbstractVector{T}}, A::SparseVector) = convert(SparseVector{T}, A)
Base.convert(::Type{Vector}, S::SparseVector) = full(S)

function Base.full{Tv}(S::SparseVector{Tv})
    A = zeros(Tv, S.m)
    for m = 1:length(S.rowval)
        A[S.rowval[m]] = S.nzval[m]
    end
    return A
end

Base.float(S::SparseVector) = SparseVector(S.m, copy(S.rowval), float(copy(S.nzval)))

Base.complex(S::SparseVector) = SparseVector(S.m, copy(S.rowval), complex(copy(S.nzval)))

Base.complex(A::SparseVector, B::SparseVector) = A + im*B

#TODO: remove other definition in sparsematrix.jl
#=function Base.vec{Tv,Ti}(S::SparseMatrixCSC{Tv,Ti})
    colptr = Array(Ti,2)
    rowval = similar(S.rowval)
    lS = length(S)
    sparse_compute_reshaped_colptr_and_rowval(colptr, rowval, lS, 1, S.colptr, S.rowval, S.m, S.n)
    return SparseVector{Tv,Ti}(lS, rowval, copy(S.nzval))
end=#

#sparsevec(A::AbstractMatrix) = reshape(sparse(A), (length(A),1))
sparsevec(S::SparseMatrixCSC) = vec(S)

sparsevec{K<:Integer,V}(d::Dict{K,V}, len::Int) = sparsevec(collect(keys(d)), collect(values(d)), len)

sparsevec{K<:Integer,V}(d::Dict{K,V}) = sparsevec(collect(keys(d)), collect(values(d)))

sparsevec(I::AbstractVector, V, m::Integer) = sparsevec(I, V, m, +)

sparsevec(I::AbstractVector, V) = sparsevec(I, V, maximum(I), +)

function sparsevec(I::AbstractVector, V, m::Integer, combine::Function)
    nI = length(I)
    if isa(V, Number); V = fill(V, nI); end
    p = sortperm(I)
    @inbounds I = I[p]
    (nI==0 || m >= I[end]) || throw(DimensionMismatch("indices cannot be larger than length of vector"))
    (nI==0 || I[1] > 0) || throw(BoundsError())
    V = V[p]
    sparse_I_sorted!(I, V, m, combine)
end

function sparsevec(V::Vector)
    m = length(V)
    I, V = findnz(V)
    return sparse_I_sorted!(I,V,m,+)
end

sparse(a::Vector) = sparsevec(a)

function sprand{T}(m::Integer, density::FloatingPoint,
                   rng::Function,::Type{T}=eltype(rng(1)))
    0 <= density <= 1 || throw(ArgumentError("$density not in [0,1]"))
    m == 0 && return spzeros(T,m)
    # TODO: need to change rng(1)?
    m == 1 && return rand() <= density ? sparse(rng(1)) : spzeros(T,1)

    I = Array(Int, 0) # indices of nonzero elements
    sizehint(I, int(N*density))

    # density of nonzero columns:
    L = log1p(-density)
    coldensity = -expm1(m*L) # = 1 - (1-density)^m 
    colsparsity = exp(m*L) # = 1 - coldensity
    L = 1/L
    
    rows = Array(Int, 0)
    for j in randsubseq(1:n, coldensity)
        # To get the right statistics, we *must* have a nonempty column j
        # even if p*m << 1.   To do this, we use an approach similar to
        # the one in randsubseq to compute the expected first nonzero row k,
        # except given that at least one is nonzero (via Bayes' rule);
        # carefully rearranged to avoid excessive roundoff errors.
        k = ceil(log(colsparsity + rand()*coldensity) * L)
        ik = k < 1 ? 1 : k > m ? m : int(k) # roundoff-error/underflow paranoia
        randsubseq!(rows, 1:m-ik, density)
        push!(rows, m-ik+1)
        append!(I, rows)
    end
    return sparse_I_sorted!(I, rng(length(I)), m, +)  # it will never need to combine
end

sprand(m::Integer, density::FloatingPoint) = sprand(m,density,rand,Float64)
sprandn(m::Integer, density::FloatingPoint) = sprand(m,density,randn,Float64)
sprandbool(m::Integer, density::FloatingPoint) = sprand(m,density,Base.SparseMatrix.truebools,Bool)

spones{T}(S::SparseVector{T}) =
     SparseVector(S.m, copy(S.rowval), ones(T, length(S.rowval)))

spzeros(m::Integer) = spzeros(Float64, m)
spzeros(Tv::Type, m::Integer) =
    SparseVector(m, Array(Int, 0), Array(Tv, 0))
spzeros(Tv::Type, Ti::Type, m::Integer) =
    SparseVector(m, Array(Ti, 0), Array(Tv, 0))


#TODO
 # make findnz(::Vector) == find in Base
 #