### common.jl

# not exported, used mainly for testing

_copy_convert{T}(::Type{T}, x::Vector{T}) = copy(x)
_copy_convert{R,T}(::Type{R}, x::AbstractVector{T}) = convert(Vector{R}, x)

import Base: Func, AddFun, MulFun, MaxFun, MinFun

if isdefined(Base, :call)
    import Base: call
else
    call(f::Function, x) = f(x)
    call(f::Function, x, y) = f(x, y)
    call(f::Func{1}, x) = Base.evaluate(f, x)
    call(f::Func{2}, x, y) = Base.evaluate(f, x, y)
end

if isdefined(Base, :SubFun)
    import Base: SubFun
else
    immutable SubFun <: Func{2} end
    call(::SubFun, x, y) = x - y
end

immutable RealFun <: Func{1} end
call(::RealFun, x) = real(x)

immutable ImagFun <: Func{1} end
call(::ImagFun, x) = imag(x)

immutable ComplexFun <: Func{2} end
call(::ComplexFun, x::Real, y::Real) = complex(x, y)

immutable DotFun <: Func{2} end
_dot(x::Number, y::Number) = conj(x) * y
_dot(x::Real, y::Real) = x * y
call(::DotFun, x::Number, y::Number) = _dot(x, y)

typealias UnaryOp Union(Function, Func{1})
typealias BinaryOp Union(Function, Func{2})


### sparsevec.jl

### Types

immutable SparseVector{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    n::Int              # the number of elements
    nzind::Vector{Ti}   # the indices of nonzeros
    nzval::Vector{Tv}   # the values of nonzeros

    function SparseVector(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv})
        n >= 0 || throw(ArgumentError("The number of elements must be non-negative."))
        length(nzind) == length(nzval) ||
            throw(DimensionMismatch("The lengths of nzind and nzval are inconsistent."))
        new(convert(Int, n), nzind, nzval)
    end
end

SparseVector{Tv,Ti}(n::Integer, nzind::Vector{Ti}, nzval::Vector{Tv}) =
    SparseVector{Tv,Ti}(n, nzind, nzval)

### Basic properties

length(x::SparseVector) = x.n
size(x::SparseVector) = (x.n,)
nnz(x::SparseVector) = length(x.nzval)
countnz(x::SparseVector) = countnz(x.nzval)
nonzeros(x::SparseVector) = x.nzval
nonzeroinds(x::SparseVector) = x.nzind

### Construct empty sparse vector

sparsevector{T}(::Type{T}, len::Integer) = SparseVector(len, Int[], T[])

### Construction from lists of indices and values

function _sparsevector!{Ti<:Integer}(I::Vector{Ti}, V::Vector, len::Integer)
    # pre-condition: no duplicate indexes in I
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
    end
    SparseVector(len, I, V)
end

function _sparsevector!{Tv,Ti<:Integer}(I::Vector{Ti}, V::Vector{Tv}, len::Integer, combine::BinaryOp)
    if !isempty(I)
        p = sortperm(I)
        permute!(I, p)
        permute!(V, p)
        m = length(I)

        # advances to the first non-zero element
        r = 1     # index of last processed entry
        while r <= m
            if V[r] == zero(Tv)
                r += 1
            else
                break
            end
        end
        r > m && SparseVector(len, Ti[], Tv[])

        # move r-th to l-th
        l = 1       # length of processed part
        i = I[r]    # row-index of current element
        if r > 1
            I[l] = i; V[l] = V[r]
        end

        # main loop
        while r < m
            r += 1
            i2 = I[r]
            if i2 == i  # accumulate r-th to the l-th entry
                V[l] = call(combine, V[l], V[r])
            else  # advance l, and move r-th to l-th
                pv = V[l]
                if pv != zero(Tv)
                    l += 1
                end
                i = i2
                if l < r
                    I[l] = i; V[l] = V[r]
                end
            end
        end
        if V[l] == zero(Tv)
            l -= 1
        end
        if l < m
            resize!(I, l)
            resize!(V, l)
        end
    end
    SparseVector(len, I, V)
end

function sparsevector{Tv,Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector{Tv}, combine::BinaryOp)
    length(I) == length(V) ||
        throw(DimensionMismatch("The lengths of I and V are inconsistent."))
    len = 0
    for i in I
        i >= 1 || error("Index must be positive.")
        if i > len
            len = i
        end
    end
    _sparsevector!(_copy_convert(Ti, I), _copy_convert(Tv, V), len, combine)
end

function sparsevector{Tv,Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector{Tv}, len::Integer, combine::BinaryOp)
    length(I) == length(V) ||
        throw(DimensionMismatch("The lengths of I and V are inconsistent."))
    maxi = convert(Ti, len)
    for i in I
        1 <= i <= maxi || error("An index is out of bound.")
    end
    _sparsevector!(_copy_convert(Ti, I), _copy_convert(Tv, V), len, combine)
end

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector) =
    sparsevector(I, V, AddFun())

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, V::AbstractVector, len::Integer) =
    sparsevector(I, V, len, AddFun())

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, combine::BinaryOp) =
    sparsevector(I, fill(v, length(I)), combine)

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, len::Integer, combine::BinaryOp) =
    sparsevector(I, fill(v, length(I)), len, combine)

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, v::Number) =
    sparsevector(I, v, AddFun())

sparsevector{Ti<:Integer}(I::AbstractVector{Ti}, v::Number, len::Integer) =
    sparsevector(I, v, len, AddFun())


### Construction from dictionary

function sparsevector{Tv,Ti<:Integer}(dict::Associative{Ti,Tv})
    m = length(dict)
    nzind = Array(Ti, m)
    nzval = Array(Tv, m)

    cnt = 0
    len = zero(Ti)
    for (k, v) in dict
        k >= 1 || error("Index must be positive.")
        if k > len
            len = k
        end
        if v != zero(v)
            cnt += 1
            @inbounds nzind[cnt] = k
            @inbounds nzval[cnt] = v
        end
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end

function sparsevector{Tv,Ti<:Integer}(dict::Associative{Ti,Tv}, len::Integer)
    m = length(dict)
    nzind = Array(Ti, m)
    nzval = Array(Tv, m)

    cnt = 0
    maxk = convert(Ti, len)
    for (k, v) in dict
        1 <= k <= maxk || error("An index (key) is out of bound.")
        if v != zero(v)
            cnt += 1
            @inbounds nzind[cnt] = k
            @inbounds nzval[cnt] = v
        end
    end
    resize!(nzind, cnt)
    resize!(nzval, cnt)
    _sparsevector!(nzind, nzval, len)
end


### Element access

function setindex!{Tv,Ti<:Integer}(x::SparseVector{Tv,Ti}, v::Tv, i::Ti)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)

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
    setindex!(x, convert(Tv, v), convert(Ti, i))


### Conversion

# convert SparseMatrixCSC to SparseVector
function convert{Tv,Ti<:Integer}(::Type{SparseVector{Tv,Ti}}, s::SparseMatrixCSC{Tv,Ti})
    size(s, 2) == 1 || throw(ArgumentError("The input argument must have a single-column."))
    SparseVector(s.m, s.rowval, s.nzval)
end

convert{Tv,Ti}(::Type{SparseVector{Tv}}, s::SparseMatrixCSC{Tv,Ti}) =
    convert(SparseVector{Tv,Ti}, s)

convert{Tv,Ti}(::Type{SparseVector}, s::SparseMatrixCSC{Tv,Ti}) =
    convert(SparseVector{Tv,Ti}, s)

# convert Vector to SparseVector

function _dense2sparsevec{Tv}(s::Vector{Tv}, initcap::Int)
    # pre-condition: initcap > 0
    n = length(s)
    cap = initcap
    nzind = Array(Int, cap)
    nzval = Array(Tv, cap)
    c = 0
    @inbounds for i = 1:n
        v = s[i]
        if v != zero(v)
            if c >= cap
                cap *= 2
                resize!(nzind, cap)
                resize!(nzval, cap)
            end
            c += 1
            nzind[c] = i
            nzval[c] = v
        end
    end
    if c < cap
        resize!(nzind, c)
        resize!(nzval, c)
    end
    SparseVector(n, nzind, nzval)
end

convert{Tv}(::Type{SparseVector{Tv,Int}}, s::Vector{Tv}) =
    _dense2sparsevec(s, max(8, div(length(s), 8)))

convert{Tv}(::Type{SparseVector{Tv}}, s::Vector{Tv}) =
    convert(SparseVector{Tv,Int}, s)

convert{Tv}(::Type{SparseVector}, s::Vector{Tv}) =
    convert(SparseVector{Tv,Int}, s)


# convert between different types of SparseVector
convert{Tv,Ti,TvS,TiS}(::Type{SparseVector{Tv,Ti}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,Ti}(s.n, convert(Vector{Ti}, s.nzind), convert(Vector{Tv}, s.nzval))

convert{Tv,TvS,TiS}(::Type{SparseVector{Tv}}, s::SparseVector{TvS,TiS}) =
    SparseVector{Tv,TiS}(s.n, s.nzind, convert(Vector{Tv}, s.nzval))


### Rand Construction

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


### sparsevecview.jl

using ArrayViews
import ArrayViews: view

typealias CVecView{T} ContiguousView{T,1,Vector{T}}

immutable SparseVectorView{Tv,Ti<:Integer} <: AbstractSparseVector{Tv,Ti}
    n::Int                  # the number of elements
    nzind::CVecView{Ti}     # the indices of nonzeros
    nzval::CVecView{Tv}     # the values of nonzeros

    function SparseVectorView(n::Integer, nzind::CVecView{Ti}, nzval::CVecView{Tv})
        n >= 0 || throw(ArgumentError("The number of elements must be non-negative."))
        length(nzind) == length(nzval) ||
            throw(DimensionMismatch("The lengths of nzind and nzval are inconsistent."))
        new(convert(Int, n), nzind, nzval)
    end
end

### Construction

SparseVectorView{Tv,Ti}(n::Integer, nzind::CVecView{Ti}, nzval::CVecView{Tv}) =
    SparseVectorView{Tv,Ti}(n, nzind, nzval)

view(x::AbstractSparseVector) =
    SparseVectorView(length(x), view(nonzeroinds(x)), view(nonzeros(x)))

### Basic properties

length(x::SparseVectorView) = x.n
size(x::SparseVectorView) = (x.n,)
nnz(x::SparseVectorView) = length(x.nzval)
countnz(x::SparseVectorView) = countnz(x.nzval)
nonzeros(x::SparseVectorView) = x.nzval
nonzeroinds(x::SparseVectorView) = x.nzind

### sparsematview.jl


### Views

jvec_rgn(x::Vector, first::Int, n::Int) = pointer_to_array(pointer(x, first), n, false)

as_jvec{T}(x::ContiguousView{T,1,Vector{T}}) = pointer_to_array(pointer(x), length(x), false)

function view(x::SparseMatrixCSC, ::Colon, j::Integer)
    1 <= j <= x.n || throw(BoundsError())
    r1 = convert(Int, x.colptr[j])
    r2 = convert(Int, x.colptr[j+1]) - 1
    rgn = r1:r2
    SparseVectorView(x.m, view(x.rowval, rgn), view(x.nzval, rgn))
end

function getcol(x::SparseMatrixCSC, j::Integer)
    1 <= j <= x.n || throw(BoundsError())
    r1 = convert(Int, x.colptr[j])
    r2 = convert(Int, x.colptr[j+1]) - 1
    SparseVector(x.m, x.rowval[r1:r2], x.nzval[r1:r2])
end

function unsafe_colrange{Tv,Ti}(x::SparseMatrixCSC{Tv,Ti}, J::UnitRange)
    jfirst = first(J)
    jlast = last(J)
    (1 <= jfirst <= x.n && jlast <= x.n) || throw(BoundsError())
    r1 = x.colptr[jfirst]
    r2 = x.colptr[jlast+1] - one(r1)
    newcolptr = view(x.colptr, jfirst:jlast+1) - (r1 - one(r1))

    fi = convert(Int, r1)
    nc = convert(Int, r2 - r1) + 1
    SparseMatrixCSC{Tv, Ti}(x.m, length(J), newcolptr,
        jvec_rgn(x.rowval, fi, nc), jvec_rgn(x.nzval, fi, nc))
end

### generics.jl
# Generic functions operating on AbstractSparseVector

### getindex

function _spgetindex{Tv,Ti}(m::Int, nzind::AbstractVector{Ti}, nzval::AbstractVector{Tv}, i::Integer)
    ii = searchsortedfirst(nzind, convert(Ti, i))
    (ii <= m && nzind[ii] == i) ? nzval[ii] : zero(Tv)
end

getindex{Tv}(x::AbstractSparseVector{Tv}, i::Integer) =
    _spgetindex(nnz(x), nonzeroinds(x), nonzeros(x), i)

function getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::UnitRange)
    xlen = length(x)
    i0 = first(I)
    i1 = last(I)
    (i0 >= 1 && i1 <= xlen) || throw(BoundsError())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)

    # locate the first j0, s.t. xnzind[j0] >= i0
    j0 = 1
    while j0 <= m && xnzind[j0] < i0
        j0 += 1
    end
    # locate the last j1, s.t. xnzind[j1] <= i1
    j1 = j0 - 1
    while j1 < m && xnzind[j1+1] <= i1
        j1 += 1
    end

    # compute the number of non-zeros
    jrgn = j0:j1
    mr = length(jrgn)
    rind = Array(Ti, mr)
    rval = Array(Tv, mr)
    if mr > 0
        c = 0
        for j in jrgn
            c += 1
            rind[c] = convert(Ti, xnzind[j] - i0 + 1)
            rval[c] = xnzval[j]
        end
    end
    SparseVector(length(I), rind, rval)
end

function getindex{Tv,Ti}(x::AbstractSparseVector{Tv,Ti}, I::AbstractArray)
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)
    n = length(I)
    nzind = Array(Ti, n)
    nzval = Array(Tv, n)
    c = 0
    for j = 1:n
        v = _spgetindex(m, xnzind, xnzval, I[j])
        if v != zero(v)
            c += 1
            nzind[c] = convert(Ti, j)
            nzval[c] = v
        end
    end
    if c < n
        resize!(nzind, c)
        resize!(nzval, c)
    end
    SparseVector(n, nzind, nzval)
end

### show and friends

function showarray(io::IO, x::AbstractSparseVector;
                   header::Bool=true, limit::Bool=Base._limit_output,
                   rows = Base.tty_size()[1], repr=false)

    n = length(x)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    xnnz = length(nzind)

    if header
        print(io, "Sparse vector, length = ", n,
            ", with ", xnnz, " ", eltype(nzval), " entries:", "\n")
    end
    half_screen_rows = limit ? div(rows - 8, 2) : typemax(Int)
    pad = ndigits(n)
    k = 0
    sep = "\n\t"
    for k = 1:length(nzind)
        if k < half_screen_rows || k > xnnz - half_screen_rows
            print(io, "  ", '[', rpad(nzind[k], pad), "]  =  ")
            showcompact(io, nzval[k])
        elseif k == half_screen_rows
            print(io, sep, '\u22ee')
        end
        print(io, "\n")
        k += 1
    end
end

show(io::IO, x::AbstractSparseVector) = showarray(io, x)
writemime(io::IO, ::MIME"text/plain", x::AbstractSparseVector) = show(io, x)


### Comparison

function exact_equal(x::AbstractSparseVector, y::AbstractSparseVector)
    eltype(x) == eltype(y) &&
    eltype(nonzeroinds(x)) == eltype(nonzeroinds(y)) &&
    length(x) == length(y) &&
    nonzeroinds(x) == nonzeroinds(y) &&
    nonzeros(x) == nonzeros(y)
end

### Conversion to matrix

function convert{TvD,TiD,Tv,Ti}(::Type{SparseMatrixCSC{TvD,TiD}}, x::AbstractSparseVector{Tv,Ti})
    n = length(x)
    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)
    colptr = TiD[1, m+1]
    rowval = _copy_convert(TiD, xnzind)
    nzval = _copy_convert(TvD, xnzval)
    SparseMatrixCSC(n, 1, colptr, rowval, nzval)
end

convert{TvD,Tv,Ti}(::Type{SparseMatrixCSC{TvD}}, x::AbstractSparseVector{Tv,Ti}) =
    convert(SparseMatrixCSC{TvD,Ti}, x)

convert{Tv,Ti}(::Type{SparseMatrixCSC}, x::AbstractSparseVector{Tv,Ti}) =
    convert(SparseMatrixCSC{Tv,Ti}, x)


### Array manipulation

function full{Tv}(x::AbstractSparseVector{Tv})
    n = length(x)
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    r = zeros(Tv, n)
    for i = 1:length(nzind)
        r[nzind[i]] = nzval[i]
    end
    return r
end

vec(x::AbstractSparseVector) = x
copy(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), copy(nonzeros(x)))

function reinterpret{T,Tv}(::Type{T}, x::AbstractSparseVector{Tv})
    sizeof(T) == sizeof(Tv) ||
        throw(ArgumentError("reinterpret of sparse vectors only supports element types of the same size."))
    SparseVector(length(x), copy(nonzeroinds(x)), reinterpret(T, nonzeros(x)))
end

float{Tv<:FloatingPoint}(x::AbstractSparseVector{Tv}) = x
float(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), float(nonzeros(x)))

complex{Tv<:Complex}(x::AbstractSparseVector{Tv}) = x
complex(x::AbstractSparseVector) =
    SparseVector(length(x), copy(nonzeroinds(x)), complex(nonzeros(x)))


### Concatenation

function hcat{Tv,Ti}(X::AbstractSparseVector{Tv,Ti}...)
    # check sizes
    n = length(X)
    m = length(X[1])
    tnnz = nnz(X[1])
    for j = 2:n
        length(X[j]) == m ||
            throw(DimensionMismatch("Inconsistent column lengths."))
        tnnz += nnz(X[j])
    end

    # construction
    colptr = Array(Ti, n+1)
    nzrow = Array(Ti, tnnz)
    nzval = Array(Tv, tnnz)
    roff = 1
    @inbounds for j = 1:n
        xj = X[j]
        xnzind = nonzeroinds(xj)
        xnzval = nonzeros(xj)
        colptr[j] = roff
        copy!(nzrow, roff, xnzind)
        copy!(nzval, roff, xnzval)
        roff += length(xnzind)
    end
    colptr[n+1] = roff
    SparseMatrixCSC{Tv,Ti}(m, n, colptr, nzrow, nzval)
end

function vcat{Tv,Ti}(X::AbstractSparseVector{Tv,Ti}...)
    # check sizes
    n = length(X)
    tnnz = 0
    for j = 1:n
        tnnz += nnz(X[j])
    end

    # construction
    rnzind = Array(Ti, tnnz)
    rnzval = Array(Tv, tnnz)
    ir = 0
    len = 0
    @inbounds for j = 1:n
        xj = X[j]
        xnzind = nonzeroinds(xj)
        xnzval = nonzeros(xj)
        xnnz = length(xnzind)
        for i = 1:xnnz
            rnzind[ir + i] = xnzind[i] + len
        end
        copy!(rnzval, ir+1, xnzval)
        ir += xnnz
        len += length(xj)
    end
    SparseVector(len, rnzind, rnzval)
end

### math.jl


### Unary Map

# zero-preserving functions (z->z, nz->nz)
for op in [:-, :abs, :abs2, :conj]
    @eval begin
        $(op)(x::AbstractSparseVector) =
            SparseVector(length(x), copy(nonzeroinds(x)), $(op)(nonzeros(x)))
    end
end

# functions f, such that
#   f(x) can be zero or non-zero when x != 0
#   f(x) = 0 when x == 0
#
macro unarymap_nz2z_z2z(op, TF)
    esc(quote
        function $(op){Tv<:$(TF),Ti<:Integer}(x::AbstractSparseVector{Tv,Ti})
            R = typeof($(op)(zero(Tv)))
            xnzind = nonzeroinds(x)
            xnzval = nonzeros(x)
            m = length(xnzind)

            ynzind = Array(Ti, m)
            ynzval = Array(R, m)
            ir = 0
            @inbounds for j = 1:m
                i = xnzind[j]
                v = $(op)(xnzval[j])
                if v != zero(v)
                    ir += 1
                    ynzind[ir] = i
                    ynzval[ir] = v
                end
            end
            resize!(ynzind, ir)
            resize!(ynzval, ir)
            SparseVector(length(x), ynzind, ynzval)
        end
    end)
end

real{T<:Real}(x::AbstractSparseVector{T}) = x
@unarymap_nz2z_z2z real Complex

imag{Tv<:Real,Ti<:Integer}(x::AbstractSparseVector{Tv,Ti}) = SparseVector(length(x), Ti[], Tv[])
@unarymap_nz2z_z2z imag Complex

for op in [:floor, :ceil, :trunc, :round]
    @eval @unarymap_nz2z_z2z $(op) Real
end

for op in [:log1p, :expm1,
           :sin, :tan, :sinpi, :sind, :tand,
           :asin, :atan, :asind, :atand,
           :sinh, :tanh, :asinh, :atanh]
    @eval @unarymap_nz2z_z2z $(op) Number
end

# function that does not preserve zeros

macro unarymap_z2nz(op, TF)
    esc(quote
        function $(op){Tv<:$(TF),Ti<:Integer}(x::AbstractSparseVector{Tv,Ti})
            v0 = $(op)(zero(Tv))
            R = typeof(v0)
            xnzind = nonzeroinds(x)
            xnzval = nonzeros(x)
            n = length(x)
            m = length(xnzind)
            y = fill(v0, n)
            @inbounds for j = 1:m
                y[xnzind[j]] = $(op)(xnzval[j])
            end
            y
        end
    end)
end

for op in [:exp, :exp2, :exp10, :log, :log2, :log10,
           :cos, :csc, :cot, :sec, :cospi,
           :cosd, :cscd, :cotd, :secd,
           :acos, :acot, :acosd, :acotd,
           :cosh, :csch, :coth, :sech,
           :acsch, :asech]
    @eval @unarymap_z2nz $(op) Number
end


### Binary Map

# mode:
# 0: f(nz, nz) -> nz, f(z, nz) -> z, f(nz, z) ->  z
# 1: f(nz, nz) -> z/nz, f(z, nz) -> nz, f(nz, z) -> nz
# 2: f(nz, nz) -> z/nz, f(z, nz) -> z/nz, f(nz, z) -> z/nz

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractSparseVector{Tx},
                           y::AbstractSparseVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(call(f, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    mx = length(xnzind)
    my = length(ynzind)
    (mx == 0 || my == 0) && return SparseVector(R, 0)
    cap = (mode == 0 ? min(mx, my) : mx + my)::Int

    rind = Array(Int, cap)
    rval = Array(R, cap)
    ir = 0
    ix = 1
    iy = 1

    ir = (
        mode == 0 ? _binarymap_mode_0!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval) :
        mode == 1 ? _binarymap_mode_1!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval) :
        _binarymap_mode_2!(f, mx, my,
            xnzind, xnzval, ynzind, ynzval, rind, rval)
    )::Int

    resize!(rind, ir)
    resize!(rval, ir)
    return SparseVector(n, rind, rval)
end

function _binarymap_mode_0!(f::BinaryOp, mx::Int, my::Int,
                            xnzind, xnzval, ynzind, ynzval, rind, rval)
    # f(nz, nz) -> nz, f(z, nz) -> z, f(nz, z) ->  z
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = call(f, xnzval[ix], ynzval[iy])
            ir += 1; rind[ir] = jx; rval[ir] = v
            ix += 1; iy += 1
        elseif jx < jy
            ix += 1
        else
            iy += 1
        end
    end
    return ir
end

function _binarymap_mode_1!{Tx,Ty}(f::BinaryOp, mx::Int, my::Int,
                                   xnzind, xnzval::AbstractVector{Tx},
                                   ynzind, ynzval::AbstractVector{Ty},
                                   rind, rval)
    # f(nz, nz) -> z/nz, f(z, nz) -> nz, f(nz, z) -> nz
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = call(f, xnzval[ix], ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1; iy += 1
        elseif jx < jy
            v = call(f, xnzval[ix], zero(Ty))
            ir += 1; rind[ir] = jx; rval[ir] = v
            ix += 1
        else
            v = call(f, zero(Tx), ynzval[iy])
            ir += 1; rind[ir] = jy; rval[ir] = v
            iy += 1
        end
    end
    @inbounds while ix <= mx
        v = call(f, xnzval[ix], zero(Ty))
        ir += 1; rind[ir] = xnzind[ix]; rval[ir] = v
        ix += 1
    end
    @inbounds while iy <= my
        v = call(f, zero(Tx), ynzval[iy])
        ir += 1; rind[ir] = ynzind[iy]; rval[ir] = v
        iy += 1
    end
    return ir
end

function _binarymap_mode_2!{Tx,Ty}(f::BinaryOp, mx::Int, my::Int,
                                   xnzind, xnzval::AbstractVector{Tx},
                                   ynzind, ynzval::AbstractVector{Ty},
                                   rind, rval)
    # f(nz, nz) -> z/nz, f(z, nz) -> z/nz, f(nz, z) -> z/nz
    ir = 0; ix = 1; iy = 1
    @inbounds while ix <= mx && iy <= my
        jx = xnzind[ix]
        jy = ynzind[iy]
        if jx == jy
            v = call(f, xnzval[ix], ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1; iy += 1
        elseif jx < jy
            v = call(f, xnzval[ix], zero(Ty))
            if v != zero(v)
                ir += 1; rind[ir] = jx; rval[ir] = v
            end
            ix += 1
        else
            v = call(f, zero(Tx), ynzval[iy])
            if v != zero(v)
                ir += 1; rind[ir] = jy; rval[ir] = v
            end
            iy += 1
        end
    end
    @inbounds while ix <= mx
        v = call(f, xnzval[ix], zero(Ty))
        if v != zero(v)
            ir += 1; rind[ir] = xnzind[ix]; rval[ir] = v
        end
        ix += 1
    end
    @inbounds while iy <= my
        v = call(f, zero(Tx), ynzval[iy])
        if v != zero(v)
            ir += 1; rind[ir] = ynzind[iy]; rval[ir] = v
        end
        iy += 1
    end
    return ir
end

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractVector{Tx},
                           y::AbstractSparseVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(call(f, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    ynzind = nonzeroinds(y)
    ynzval = nonzeros(y)
    m = length(ynzind)

    dst = Array(R, n)
    if mode == 0
        ii = 1
        @inbounds for i = 1:m
            j = ynzind[i]
            while ii < j
                dst[ii] = zero(R); ii += 1
            end
            dst[j] = call(f, x[j], ynzval[i]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = zero(R); ii += 1
        end
    else # mode >= 1
        ii = 1
        @inbounds for i = 1:m
            j = ynzind[i]
            while ii < j
                dst[ii] = call(f, x[ii], zero(Ty)); ii += 1
            end
            dst[j] = call(f, x[j], ynzval[i]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = call(f, x[ii], zero(Ty)); ii += 1
        end
    end
    return dst
end

function _binarymap{Tx,Ty}(f::BinaryOp,
                           x::AbstractSparseVector{Tx},
                           y::AbstractVector{Ty},
                           mode::Int)

    0 <= mode <= 2 || throw(ArgumentError("Incorrect mode $mode."))
    R = typeof(call(f, zero(Tx), zero(Ty)))
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    m = length(xnzind)

    dst = Array(R, n)
    if mode == 0
        ii = 1
        @inbounds for i = 1:m
            j = xnzind[i]
            while ii < j
                dst[ii] = zero(R); ii += 1
            end
            dst[j] = call(f, xnzval[i], y[j]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = zero(R); ii += 1
        end
    else # mode >= 1
        ii = 1
        @inbounds for i = 1:m
            j = xnzind[i]
            while ii < j
                dst[ii] = call(f, zero(Tx), y[ii]); ii += 1
            end
            dst[j] = call(f, xnzval[i], y[j]); ii += 1
        end
        @inbounds while ii <= n
            dst[ii] = call(f, zero(Tx), y[ii]); ii += 1
        end
    end
    return dst
end


### Binary arithmetics: +, -, *

for (vop, fun, mode) in [(:_vadd, :AddFun, 1),
                         (:_vsub, :SubFun, 1),
                         (:_vmul, :MulFun, 0)]
    @eval begin
        $(vop)(x::AbstractSparseVector, y::AbstractSparseVector) = _binarymap($(fun)(), x, y, $mode)
        $(vop)(x::StridedVector, y::AbstractSparseVector) = _binarymap($(fun)(), x, y, $mode)
        $(vop)(x::AbstractSparseVector, y::StridedVector) = _binarymap($(fun)(), x, y, $mode)
    end
end

# to workaround the ambiguities with vectorized dates/arithmetic.jl functions
if VERSION > v"0.4-dev"
    +{T<:Dates.TimeType,P<:Dates.GeneralPeriod}(x::StridedVector{P}, y::AbstractSparseVector{T}) = _vadd(x, y)
    -{T<:Dates.TimeType,P<:Dates.GeneralPeriod}(x::StridedVector{P}, y::AbstractSparseVector{T}) = _vsub(x, y)
    +{T<:Dates.TimeType,P<:Dates.GeneralPeriod}(x::AbstractSparseVector{T}, y::StridedVector{P}) = _vadd(x, y)
    -{T<:Dates.TimeType,P<:Dates.GeneralPeriod}(x::AbstractSparseVector{T}, y::StridedVector{P}) = _vsub(x, y)
end

# to workaround the ambiguities with BitVector
.*(x::BitVector, y::AbstractSparseVector{Bool}) = _vmul(x, y)
.*(x::AbstractSparseVector{Bool}, y::BitVector) = _vmul(x, y)

# definition of operators

for (op, vop) in [(:+, :_vadd), (:(.+), :_vadd),
                  (:-, :_vsub), (:(.-), :_vsub),
                  (:.*, :_vmul)]
    @eval begin
        $(op)(x::AbstractSparseVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::StridedVector, y::AbstractSparseVector) = $(vop)(x, y)
        $(op)(x::AbstractSparseVector, y::StridedVector) = $(vop)(x, y)
    end
end

# definition of other binary functions

for (op, fun, TF, mode) in [(:max, :MaxFun, :Real, 2),
                            (:min, :MinFun, :Real, 2),
                            (:complex, :ComplexFun, :Real, 1)]
    @eval begin
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::AbstractSparseVector{Tx}, y::AbstractSparseVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::StridedVector{Tx}, y::AbstractSparseVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
        $(op){Tx<:$(TF),Ty<:$(TF)}(x::AbstractSparseVector{Tx}, y::StridedVector{Ty}) =
            _binarymap($(fun)(), x, y, $mode)
    end
end

### Reduction

sum(x::AbstractSparseVector) = sum(nonzeros(x))
sumabs(x::AbstractSparseVector) = sumabs(nonzeros(x))
sumabs2(x::AbstractSparseVector) = sumabs2(nonzeros(x))

function maximum{T<:Real}(x::AbstractSparseVector{T})
    n = length(x)
    n > 0 || throw(ArgumentError("maximum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? maximum(nonzeros(x)) :
     max(zero(T), maximum(nonzeros(x))))::T
end

function minimum{T<:Real}(x::AbstractSparseVector{T})
    n = length(x)
    n > 0 || throw(ArgumentError("minimum over empty array is not allowed."))
    m = nnz(x)
    (m == 0 ? zero(T) :
     m == n ? minimum(nonzeros(x)) :
     min(zero(T), minimum(nonzeros(x))))::T
end

maxabs{T<:Number}(x::AbstractSparseVector{T}) = maxabs(nonzeros(x))
minabs{T<:Number}(x::AbstractSparseVector{T}) = nnz(x) < length(x) ? abs(zero(T)) : minabs(nonzeros(x))

vecnorm(x::AbstractSparseVector, p::Real=2) = vecnorm(nonzeros(x), p)

### linalg.jl


### BLAS Level-1

# axpy

function LinAlg.axpy!(a::Number, x::AbstractSparseVector, y::StridedVector)
    length(x) == length(y) || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    m = length(nzind)

    if a == one(a)
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] += v
        end
    elseif a == -one(a)
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] -= v
        end
    else
        for i = 1:m
            @inbounds ii = nzind[i]
            @inbounds v = nzval[i]
            y[ii] += a * v
        end
    end
    return y
end


# scale

scale!(x::AbstractSparseVector, a::Real) = (scale!(nonzeros(x), a); x)
scale!(x::AbstractSparseVector, a::Complex) = (scale!(nonzeros(x), a); x)
scale!(a::Real, x::AbstractSparseVector) = scale!(nonzeros(x), a)
scale!(a::Complex, x::AbstractSparseVector) = scale!(nonzeros(x), a)

scale(x::AbstractSparseVector, a::Real) =
    SparseVector(length(x), copy(nonzeroinds(x)), scale(nonzeros(x), a))
scale(x::AbstractSparseVector, a::Complex) =
    SparseVector(length(x), copy(nonzeroinds(x)), scale(nonzeros(x), a))

scale(a::Real, x::AbstractSparseVector) = scale(x, a)
scale(a::Complex, x::AbstractSparseVector) = scale(x, a)

*(x::AbstractSparseVector, a::Number) = scale(x, a)
*(a::Number, x::AbstractSparseVector) = scale(x, a)
.*(x::AbstractSparseVector, a::Number) = scale(x, a)
.*(a::Number, x::AbstractSparseVector) = scale(x, a)


# dot

function dot{Tx<:Number,Ty<:Number}(x::StridedVector{Tx}, y::AbstractSparseVector{Ty})
    n = length(x)
    length(y) == n || throw(DimensionMismatch())
    nzind = nonzeroinds(y)
    nzval = nonzeros(y)
    s = zero(Tx) * zero(Ty)
    for i = 1:length(nzind)
        s += _dot(x[nzind[i]], nzval[i])
    end
    return s
end

function dot{Tx<:Number,Ty<:Number}(x::AbstractSparseVector{Tx}, y::AbstractVector{Ty})
    n = length(y)
    length(x) == n || throw(DimensionMismatch())
    nzind = nonzeroinds(x)
    nzval = nonzeros(x)
    s = zero(Tx) * zero(Ty)
    for i = 1:length(nzind)
        s += _dot(nzval[i], y[nzind[i]])
    end
    return s
end

function _spdot(f::BinaryOp,
                xj::Int, xj_last::Int, xnzind, xnzval,
                yj::Int, yj_last::Int, ynzind, ynzval)
    # dot product between ranges of non-zeros,
    s = zero(eltype(xnzval)) * zero(eltype(ynzval))
    @inbounds while xj <= xj_last && yj <= yj_last
        ix = xnzind[xj]
        iy = ynzind[yj]
        if ix == iy
            s += call(f, xnzval[xj], ynzval[yj])
            xj += 1
            yj += 1
        elseif ix < iy
            xj += 1
        else
            yj += 1
        end
    end
    s
end

function dot{Tx<:Number,Ty<:Number}(x::AbstractSparseVector{Tx}, y::AbstractSparseVector{Ty})
    is(x, y) && return sumabs2(x)
    n = length(x)
    length(y) == n || throw(DimensionMismatch())

    xnzind = nonzeroinds(x)
    ynzind = nonzeroinds(y)
    xnzval = nonzeros(x)
    ynzval = nonzeros(y)

    _spdot(DotFun(),
           1, length(xnzind), xnzind, xnzval,
           1, length(ynzind), ynzind, ynzval)
end


### BLAS-2 / dense A * sparse x -> dense y

# A_mul_B

function *{Ta,Tx}(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx})
    m, n = size(A)
    length(x) == n || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Array(Ty, m)
    A_mul_B!(y, A, x)
end

A_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) =
    A_mul_B!(one(Tx), A, x, zero(Ty), y)

function A_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    @inbounds for i = 1:length(xnzind)
        v = xnzval[i]
        if v != zero(v)
            j = xnzind[i]
            αv = v * α
            for r = 1:m
                y[r] += A[r,j] * αv
            end
        end
    end
    return y
end

# At_mul_B

function At_mul_B{Ta,Tx}(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx})
    m, n = size(A)
    length(x) == m || throw(DimensionMismatch())
    Ty = promote_type(Ta, Tx)
    y = Array(Ty, n)
    At_mul_B!(y, A, x)
end

At_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) =
    At_mul_B!(one(Tx), A, x, zero(Ty), y)

function At_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    _nnz = length(xnzind)
    _nnz == 0 && return y

    s0 = zero(eltype(A)) * zero(eltype(x))
    @inbounds for j = 1:n
        s = zero(s0)
        for i = 1:_nnz
            s += A[xnzind[i], j] * xnzval[i]
        end
        y[j] += s * α
    end
    return y
end


### BLAS-2 / sparse A * sparse x -> dense y

function densemv(A::SparseMatrixCSC, x::AbstractSparseVector; trans::Char='N')
    xlen::Int
    ylen::Int
    m, n = size(A)
    if trans == 'N' || trans == 'n'
        xlen = n; ylen = m
    elseif trans == 'T' || trans == 't' || trans == 'C' || trans == 'c'
        xlen = m; ylen = n
    else
        throw(ArgumentError("Invalid trans character $trans"))
    end
    xlen == length(x) || throw(DimensionMismatch())
    T = promote_type(eltype(A), eltype(x))
    y = Array(T, ylen)
    if trans == 'N' || trans == 'N'
        A_mul_B!(y, A, x)
    elseif trans == 'T' || trans == 't'
        At_mul_B!(y, A, x)
    elseif trans == 'C' || trans == 'c'
        Ac_mul_B!(y, A, x)
    else
        throw(ArgumentError("Invalid trans character $trans"))
    end
    y
end

# A_mul_B

A_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    A_mul_B!(one(Tx), A, x, zero(Ty), y)

function A_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector, β::Number, y::StridedVector)
    m, n = size(A)
    length(x) == n && length(y) == m || throw(DimensionMismatch())
    m == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval

    @inbounds for i = 1:length(xnzind)
        v = xnzval[i]
        if v != zero(v)
            αv = v * α
            j = xnzind[i]
            for r = A.colptr[j]:(Acolptr[j+1]-1)
                y[Arowval[r]] += Anzval[r] * αv
            end
        end
    end
    return y
end

# At_mul_B

At_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    At_mul_B!(one(Tx), A, x, zero(Ty), y)

At_mul_B!{Tx,Ty}(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}, β::Number, y::StridedVector{Ty}) =
    _At_or_Ac_mul_B!(MulFun(), α, A, x, β, y)

Ac_mul_B!{Tx,Ty}(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) =
    Ac_mul_B!(one(Tx), A, x, zero(Ty), y)

Ac_mul_B!{Tx,Ty}(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}, β::Number, y::StridedVector{Ty}) =
    _At_or_Ac_mul_B!(DotFun(), α, A, x, β, y)

function _At_or_Ac_mul_B!{Tx,Ty}(tfun::BinaryOp,
                                 α::Number, A::SparseMatrixCSC, x::AbstractSparseVector{Tx},
                                 β::Number, y::StridedVector{Ty})
    m, n = size(A)
    length(x) == m && length(y) == n || throw(DimensionMismatch())
    n == 0 && return y
    if β != one(β)
        β == zero(β) ? fill!(y, zero(eltype(y))) : scale!(y, β)
    end
    α == zero(α) && return y

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval
    mx = length(xnzind)

    for j = 1:n
        # s <- dot(A[:,j], x)
        s = _spdot(tfun, Acolptr[j], Acolptr[j+1]-1, Arowval, Anzval,
                   1, mx, xnzind, xnzval)
        @inbounds y[j] += s * α
    end
    return y
end


### BLAS-2 / sparse A * sparse x -> dense y

function *(A::SparseMatrixCSC, x::AbstractSparseVector)
    y = densemv(A, x)
    initcap = min(nnz(A), size(A,1))
    _dense2sparsevec(y, initcap)
end

At_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector) =
    _At_or_Ac_mul_B(MulFun(), A, x)

Ac_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector) =
    _At_or_Ac_mul_B(DotFun(), A, x)

function _At_or_Ac_mul_B{TvA,TiA,TvX,TiX}(tfun::BinaryOp, A::SparseMatrixCSC{TvA,TiA}, x::AbstractSparseVector{TvX,TiX})
    m, n = size(A)
    length(x) == m || throw(DimensionMismatch())
    Tv = promote_type(TvA, TvX)
    Ti = promote_type(TiA, TiX)

    xnzind = nonzeroinds(x)
    xnzval = nonzeros(x)
    Acolptr = A.colptr
    Arowval = A.rowval
    Anzval = A.nzval
    mx = length(xnzind)

    ynzind = Array(Ti, n)
    ynzval = Array(Tv, n)

    jr = 0
    for j = 1:n
        s = _spdot(tfun, Acolptr[j], Acolptr[j+1]-1, Arowval, Anzval,
                   1, mx, xnzind, xnzval)
        if s != zero(s)
            jr += 1
            ynzind[jr] = j
            ynzval[jr] = s
        end
    end
    if jr < n
        resize!(ynzind, jr)
        resize!(ynzval, jr)
    end
    SparseVector(n, ynzind, ynzval)
end
