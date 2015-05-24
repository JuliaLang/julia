
# Sparse vector with compressed storage both indices and values.
#
# nzind[i] is the index of the i-th stored element, whose value is nzval[i]
#
# It assumes that nzind is in ascending order.
#
type SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    n::Int              # the number of elements
    nzind::Vector{Ti}   # the indices of nonzeros
    nzval::Vector{Tv}   # the values of nonzeros

    function SparseVector(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv})
        n >= 0 || throw(ArgumentError("The number of elements must be non-negative."))
        length(nzind) == length(nzval) ||
            throw(DimensionMismatch("The lengths of nzind and nzval are inconsistent."))
        new(Int(n), nzind, nzval)
    end
end

## Construction

SparseVector{Tv,Ti<:Integer}(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv}) =
    SparseVector{Tv,Ti}(n, nzind, nzval)

SparseVector(n::Integer) = SparseVector(n, Int[], Float64[])

SparseVector{Tv}(::Type{Tv}, n::Integer) = SparseVector(n, Int[], Tv[])

SparseVector{Tv,Ti<:Integer}(::Type{Tv}, ::Type{Ti}, n::Integer) = SparseVector(n, Ti[], Tv[])

# construct from an associative map (Ti => Tv)
function SparseVector{Tv,Ti<:Integer}(n::Integer, s::Associative{Ti,Tv})
    m = length(s)
    nzind = Array(Ti, 0)
    nzval = Array(Tv, 0)
    sizehint!(nzind, m)
    sizehint!(nzval, m)

    for (k, v) in s
        if v != zero(v)
            push!(nzind, k)
            push!(nzval, v)
        end
    end

    p = sortperm(nzind)
    permute!(nzind, p)
    permute!(nzval, p)

    return SparseVector{Tv,Ti}(Int(n), nzind, nzval)
end


## Basic properties

length(x::SparseVector) = x.n
size(x::SparseVector) = (x.n,)

nnz(x::SparseVector) = length(x.nzval)
countnz(x::SparseVector) = countnz(x.nzval)
nonzeros(x::SparseVector) = x.nzval

## Element access

function getindex{Tv}(x::SparseVector{Tv}, i::Int)
    m = length(x.nzind)
    ii = searchsortedfirst(x.nzind, i)
    (ii <= m && x.nzind[ii] == i) ? x.nzval[ii] : zero(Tv)
end

getindex(x::SparseVector, i::Integer) = x[Int(i)]

function setindex!{Tv,Ti<:Integer}(x::SparseVector{Tv,Ti}, v::Tv, i::Ti)
    nzind = x.nzind
    nzval = x.nzval

    m = length(nzind)
    k = searchsortedfirst(nzind, i)
    if 1 <= k <= m && nzind[k] == i  # i found
        if v == zero(v)
            deleteat!(nzind, k)
            deleteat!(nzval, k)
        else
            nzval[k] = v
        end
    else  # i not found
        if v != zero(v)
            insert!(nzind, k, i)
            insert!(nzval, k, v)
        end
    end
    x
end

setindex!{Tv, Ti<:Integer}(x::SparseVector{Tv,Ti}, v, i::Integer) =
    setindex!(x, convert(Tv, v), Ti(i))


## Conversion

# convert Vector to SparseVector
function convert{Tv,Ti<:Integer}(::Type{SparseVector{Tv,Ti}}, s::Vector)
    n = length(s)
    nzind = Array(Ti, 0)
    nzval = Array(Tv, 0)
    for i = 1:n
        @inbounds v = s[i]
        if v != zero(v)
            push!(nzind, convert(Ti, i))
            push!(nzval, convert(Tv, v))
        end
    end
    return SparseVector(n, nzind, nzval)
end

convert{Tv}(::Type{SparseVector{Tv}}, s::Vector{Tv}) =
    convert(SparseVector{Tv,Int}, s)

convert{Tv}(::Type{SparseVector}, s::Vector{Tv}) =
    convert(SparseVector{Tv,Int}, s)


# convert between different types of SparseVector
convert{Tv,Ti,TvS,TiS}(::Type{SparseVector{Tv,Ti}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,Ti}(s.n, convert(Vector{Ti}, s.nzind), convert(Vector{Tv}, s.nzval))

convert{Tv,TvS,TiS}(::Type{SparseVector{Tv}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,TiS}(s.n, s.nzind, convert(Vector{Tv}, s.nzval))


## Random sparse vector

function sprand{T}(n::Integer, p::FloatingPoint, rfn::Function, ::Type{T})
    I = randsubseq(1:convert(Int, n), p)
    V = rfn(T, length(I))
    SparseVector(n, I, V)
end

function sprand(n::Integer, p::FloatingPoint, rfn::Function)
    I = randsubseq(1:convert(Int, n), p)
    V = rfn(length(I))
    SparseVector(n, I, V)
end

sprand{T}(n::Integer, p::FloatingPoint, ::Type{T}) = sprand(n, p, rand, T)
sprand(n::Integer, p::FloatingPoint) = sprand(n, p, rand)

sprandn(n::Integer, p::FloatingPoint) = sprand(n, p, randn)


## Array operations

function full{Tv}(x::SparseVector{Tv})
    n = x.n
    nzind = x.nzind
    nzval = x.nzval
    r = zeros(Tv, n)
    for i = 1:length(nzind)
        r[nzind[i]] = nzval[i]
    end
    return r
end

vec(x::SparseVector) = x

copy(x::SparseVector) = SparseVector(x.n, copy(x.nzind), copy(x.nzval))


## Show

function showarray(io::IO, x::SparseVector;
                   header::Bool=true, limit::Bool=Base._limit_output,
                   rows = Base.tty_size()[1], repr=false)
    if header
        print(io, "Sparse vector, length = ", x.n,
            ", with ", nnz(x), " ", eltype(x), " entries:", "\n")
    end

    if limit
        half_screen_rows = div(rows - 8, 2)
    else
        half_screen_rows = typemax(Int)
    end
    pad = ndigits(x.n)
    k = 0

    for k = 1:length(x.nzind)
        if k < half_screen_rows || k > nnz(x)-half_screen_rows
            print(io, "\t", '[', rpad(x.nzind[k], pad), "]  =  ")
            showcompact(io, x.nzval[k])
            print(io, "\n")
        elseif k == half_screen_rows
            print(io, sep, '\u22ee')
        end
        k += 1
    end
end

show(io::IO, x::SparseVector) = Base.showarray(io, x)
writemime(io::IO, ::MIME"text/plain", x::SparseVector) = show(io, x)


## Computation

Base.abs(x::SparseVector) = SparseVector(x.n, copy(x.nzind), abs(x.nzval))
Base.abs2(x::SparseVector) = SparseVector(x.n, copy(x.nzind), abs2(x.nzval))

# zero-preserved: f(0, 0) -> 0
function zero_preserve_map{Tx,Ty}(f, x::SparseVector{Tx}, y::SparseVector{Ty})
    R = typeof(_eval(op, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = x.nzind
    xnzval = x.nzval
    ynzind = y.nzind
    ynzval = y.nzval
    mx = length(xnzind)
    my = length(ynzind)

    ix = 1
    iy = 1
    rind = Array(Int, 0)
    rval = Array(R, 0)
    sizehint!(rind, mx + my)
    sizehint!(rval, mx + my)

    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]

        if jx == jy
            v = f(xnzval[ix], ynzval[iy])
            if v != zero(v)
                push!(rind, jx)
                push!(rval, v)
            end
            ix += 1
            iy += 1
        elseif jx < jy
            v = f(xnzval[i], zero(Ty))
            if v != zero(v)
                push!(rind, jx)
                push!(rval, v)
            end
            ix += 1
        else
            v = f(zero(Tx), ynzval[iy])
            if v != zero(v)
                push!(rind, jy)
                push!(rval, v)
            end
            iy += 1
        end
    end

    @inbounds while ix <= mx
        v = f(xnzval[ix], zero(Ty))
        if v != zero(v)
            push!(rind, xnzind[ix])
            push!(rval, v)
        end
        ix += 1
    end

    @inbounds while iy <= my
        v = f(zero(Tx), ynzval[iy])
        if v != zero(v)
            push!(rind, ynzind[iy])
            push!(rval, v)
        end
        iy += 1
    end

    return SparseVector(n, rind, rval)
end

function map{Tx,Ty}(f, x::StridedVector{Tx}, y::SparseVector{Ty})
    R = typeof(_eval(op, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    ynzind = y.nzind
    ynzval = y.nzval
    m = length(ynzind)

    dst = Array(R, n)
    ii = 1
    @inbounds for i = 1:m
        j = ynzind[i]
        while ii < j
            dst[ii] = f(x[ii], zero(Ty))
            ii += 1
        end
        # at this point: ii == j
        dst[j] = f(x[j], ynzval[i])
        ii += 1
    end

    @inbounds while ii <= n
        dst[ii] = f(x[ii], zero(Ty))
        ii += 1
    end
    return dst
end

function map{Tx,Ty}(f, x::SparseVector{Tx}, y::StridedVector{Ty})
    R = typeof(_eval(op, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = x.nzind
    xnzval = x.nzval
    m = length(xnzind)

    dst = Array(R, n)
    ii = 1
    @inbounds for i = 1:m
        j = xnzind[i]
        while ii < j
            dst[ii] = f(zero(Tx), y[ii])
            ii += 1
        end
        # at this point: ii == j
        dst[j] = f(xnzval[i], y[j])
        ii += 1
    end

    @inbounds while ii <= n
        dst[ii] = f(zero(Tx), y[ii])
        ii += 1
    end
    return dst
end


+ (x::SparseVector, y::SparseVector) = zero_preserve_map(AddFun(), x, y)
- (x::SparseVector, y::SparseVector) = zero_preserve_map(SubFun(), x, y)
+ (x::StridedVector, y::SparseVector) = map(AddFun(), x, y)
- (x::StridedVector, y::SparseVector) = map(SubFun(), x, y)
+ (x::SparseVector, y::StridedVector) = map(AddFun(), x, y)
- (x::SparseVector, y::StridedVector) = map(SubFun(), x, y)

.+ (x::SparseVector, y::SparseVector) = (x + y)
.- (x::SparseVector, y::SparseVector) = (x - y)

.+ (x::StridedVector, y::SparseVector) = (x + y)
.- (x::StridedVector, y::SparseVector) = (x - y)

.+ (x::SparseVector, y::StridedVector) = (x + y)
.- (x::SparseVector, y::StridedVector) = (x - y)


sum(x::SparseVector) = sum(x.nzval)
sumabs(x::SparseVector) = sumabs(x.nzval)
sumabs2(x::SparseVector) = sumabs2(x.nzval)

vecnorm(x::SparseVector, p::Real=2) = vecnorm(x.nzval, p)
